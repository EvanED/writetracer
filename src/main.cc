#include <iostream>
#include <string>
#include <cstdlib>
#include <cassert>
#include <sstream>
#include <iomanip>

#include <PCProcess.h>
#include <Event.h>
#include <dyn_regs.h>
#include <Symtab.h>
#include <procstate.h>

#include <walker.h>
#include <frame.h>

#include <sys/syscall.h>

using namespace Dyninst;
using namespace ProcControlAPI;
using namespace Stackwalker;
using namespace SymtabAPI;
using namespace std;

Process::ptr proc;

#define printMsg(ign, f, l, args, ...)

#define BUFSIZE 1000

namespace {
    pair<string, int>
    get_source_info(Walker *proc, Address addr)
    {
        assert(proc != NULL);
        
        LibraryState *libState = proc->getProcessState()->getLibraryTracker();
        if (!libState) {
            printMsg(STAT_WARNING, __FILE__, __LINE__, "Failed to get LibraryState\n");
            return make_pair("", 0);
        }
       
        LibAddrPair lib;
        if (!libState->getLibraryAtAddr(addr, lib)) {
            printMsg(STAT_WARNING, __FILE__, __LINE__, "Failed to get library at address 0x%lx\n", addr);
            return make_pair("", 0);
        }


        Symtab *symtab;
        if (!Symtab::openFile(symtab, lib.first) || symtab == NULL) {
            printMsg(STAT_WARNING, __FILE__, __LINE__, "Symtab failed to open file %s\n", lib.first.c_str());
            return make_pair("", 0);
        }
        symtab->setTruncateLinePaths(false);
    
        Address loadAddr = lib.second;
        vector<LineNoTuple *> lines;    
        if (!symtab->getSourceLines(lines, addr - loadAddr)) {
            return make_pair("", 0);
        }

        assert(lines.size() == 1u);
        return make_pair(lines[0]->getFile(), lines[0]->getLine());
    }


    std::string
    function_name(Frame const & frame)
    {
        std::string name;
        frame.getName(name);
        return name;
    }


    Address
    address(Frame const & frame)
    {
        return frame.getRA();
    }
}


void
output_stacktrace(Process::ptr process)
{
    std::vector<Frame> stackwalk;
    Walker *walker = Walker::newWalker(process);
    assert(walker);
    walker->walkStack(stackwalk);
    for (unsigned i=0; i<stackwalk.size(); i++) {
	std::string name = function_name(stackwalk[i]);
	Address ad = address(stackwalk[i]);
	pair<string, int> src = get_source_info(walker, ad);

	std::cout << "  " << name << ": " << src.first << ":" << src.second << "\n";
    }
}

std::string
escape(char const * data)
{
    std::string s;
    while (*data) {
	if (*data >= 0x20) {
	    s.push_back(*data);
	}
	else {
	    s.push_back('\\');
	    s.push_back('x');
	    stringstream val;
	    val.setf(ios::hex);
	    val << static_cast<int>(*data);
	    s += val.str();
	}
	++data;
    }
    return s;
}


Process::cb_ret_t
handle_write(EventSyscall::const_ptr syscall,
	     int fd,
	     MachRegisterVal buf,
	     size_t count,
	     ssize_t return_maybe)
{
    Process::const_ptr process = syscall->getProcess();

    char * data = new char[count+1];
    data[count] = '\0';
    bool ok = process->readMemory(data, buf, count);
    assert(ok);

    std::cout << "write(" << fd << ", "
	      << reinterpret_cast<void*>(buf) << " [" << escape(data) << "], "
	      << count << ")";

    if (syscall->getEventType().time() == EventType::Post) {
	std::cout << " => " << return_maybe;
    }

    std::cout << "\n";

    assert(process == proc);
    output_stacktrace(proc);

    return Process::cbDefault;
}


Process::cb_ret_t
handle_syscall(int syscall_no, EventSyscall::const_ptr syscall)
{
    Thread::const_ptr thread = syscall->getThread();

    MachRegisterVal rax, rdi, rsi, rdx, rcx, r8, r9;
    bool ok = true
	&& thread->getRegister(x86_64::rax, rax)
	&& thread->getRegister(x86_64::rdi, rdi)
	&& thread->getRegister(x86_64::rsi, rsi)
	&& thread->getRegister(x86_64::rdx, rdx)
	&& thread->getRegister(x86_64::rcx, rcx)
	&& thread->getRegister(x86_64::r8, r8)
	&& thread->getRegister(x86_64::r9, r9);
    assert(ok);

    switch (syscall_no) {
    case SYS_write:
	return handle_write(syscall, rdi, rsi, rdx, rax);

    default:
	return Process::cbDefault;
    }

    (void) syscall;
}



Process::cb_ret_t on_thread_create(Event::const_ptr ev)
{
    EventSyscall::const_ptr new_thread_ev = ev->getEventSyscall();
    return handle_syscall(new_thread_ev->getSyscallNumber(), new_thread_ev);
    //return Process::cbDefault;
}

int main(int argc, char** argv)
{
    if (argc < 2) {
	cout << "Usage: " << argv[0] << " program [args]\n";
	exit(1);
    }

    vector<string> args;

    string exec = argv[1];
    for (int i=1; i<argc; ++i) {
	args.push_back(argv[i]);
    }
    proc = Process::createProcess(exec, args);

    Process::registerEventCallback(EventType::Syscall, on_thread_create);

    ThreadPool & threads = proc->threads();
    for (ThreadPool::iterator thread = threads.begin();
	 thread != threads.end(); ++thread)
    {
	(*thread)->setSyscallMode(true);
    }

    proc->continueProc();
    while (!proc->isTerminated()) {
	Process::handleEvents(true);
    }

    return 0;
}

