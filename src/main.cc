#include <iostream>
#include <string>
#include <cstdlib>
#include <cassert>
#include <sstream>

#include <PCProcess.h>
#include <Event.h>
#include <dyn_regs.h>

#include <sys/syscall.h>

using namespace Dyninst;
using namespace ProcControlAPI;
using namespace std;

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
	    cout << "*data is [" << *data << "]; in hex, " << ios::hex << static_cast<int>(*data) << "\n";
	    val << ios::hex << static_cast<int>(*data);
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
	     size_t count)
{
    Process::const_ptr process = syscall->getProcess();

    char * data = new char[count+1];
    data[count] = '\0';
    bool ok = process->readMemory(data, buf, count);
    assert(ok);

    std::cout << "write(" << fd << ", " << buf << " [" << escape(data) << "], " << count << ")\n";
    return Process::cbDefault;
}


Process::cb_ret_t
handle_syscall(int syscall_no, EventSyscall::const_ptr syscall)
{
    Thread::const_ptr thread = syscall->getThread();

    MachRegisterVal rdi, rsi, rdx, rcx, r8, r9;
    bool ok = true
	&& thread->getRegister(x86_64::rdi, rdi)
	&& thread->getRegister(x86_64::rsi, rsi)
	&& thread->getRegister(x86_64::rdx, rdx)
	&& thread->getRegister(x86_64::rcx, rcx)
	&& thread->getRegister(x86_64::r8, r8)
	&& thread->getRegister(x86_64::r9, r9);
    assert(ok);

    switch (syscall_no) {
    case SYS_write:
	return handle_write(syscall, rdi, rsi, rdx);

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
    Process::ptr proc = Process::createProcess(exec, args);

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

