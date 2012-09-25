#include <iostream>
#include <string>
#include <cstdlib>

#include <PCProcess.h>
#include <Event.h>

#include <sys/syscall.h>

using namespace Dyninst;
using namespace ProcControlAPI;
using namespace std;


Process::cb_ret_t
handle_syscall(int syscall_no, EventSyscall::const_ptr syscall)
{
    switch (syscall_no) {
    case SYS_write:
	std::cout << "write()";
	return Process::cbDefault;

    default:
	return Process::cbDefault;
    }
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
    for (unsigned i=1; i<argc; ++i) {
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

