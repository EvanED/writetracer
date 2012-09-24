#include <iostream>
#include <string>
#include <cstdlib>

#include <PCProcess.h>
#include <Event.h>

using namespace Dyninst;
using namespace ProcControlAPI;
using namespace std;

Process::cb_ret_t on_thread_create(Event::const_ptr ev)
{
    EventNewThread::const_ptr new_thread_ev = ev->getEventNewThread();
    Thread::const_ptr new_thread = new_thread_ev->getNewThread();

    cout << "New thread LWP " << new_thread->getLWP() << endl;
    return Process::cbDefault;
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

    Process::registerEventCallback(EventType::ThreadCreate, on_thread_create);

    proc->continueProc();
    while (!proc->isTerminated()) {
	Process::handleEvents(true);
    }

    return 0;
}

