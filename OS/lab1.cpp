#include <iostream>
#include <mutex>
#include <thread>
#include <chrono>
#include <condition_variable>
#include <vector>
#include <memory>
#include <queue>

using namespace std;
using namespace chrono;
using namespace this_thread;

class simple_text_buffer
{
    string content;
public:
    simple_text_buffer(string txt = "") : content(txt) {};
    void append_char() { content += 'X'; };
    string extract_content() const { return content; };
    void display() const { cout << content << endl; };
};

class synchronization_manager
{
    queue<unique_ptr<simple_text_buffer>> buffer_queue;  // Очередь для хранения всех буферов
    bool transmission_complete = false;
    mutex access_lock;
    condition_variable sync_signal;

public:
    bool check_buffer_status()
    {
        return !buffer_queue.empty() || transmission_complete;
    }

    void transmit_buffer(unique_ptr<simple_text_buffer> buffer_unit)
    {
        unique_lock<mutex> guard(access_lock);
        
        buffer_unit->append_char();
        cout << "generator_thread - ";
        buffer_unit->display();
        
        buffer_queue.push(move(buffer_unit));
        
        guard.unlock();
        sync_signal.notify_one();  // Уведомляем потребителя
        
        sleep_for(milliseconds(1000)); // Точная задержка 1 секунда
    }

    unique_ptr<simple_text_buffer> acquire_buffer()
    {
        unique_lock<mutex> guard(access_lock);
        sync_signal.wait(guard, [this] { return check_buffer_status(); });
        
        if (transmission_complete && buffer_queue.empty())
        {
            return nullptr;
        }
        
        auto buffer = move(buffer_queue.front());
        buffer_queue.pop();
        
        cout << "processor_thread - ";
        buffer->display();
        cout << endl;
        
        return buffer;
    }
    
    void signal_completion()
    {
        unique_lock<mutex> guard(access_lock);
        transmission_complete = true;
        sync_signal.notify_all();
    }
};

void data_generator(synchronization_manager& sync_handler)
{
    vector<string> message_set = {"Initial message", "Secondary data", 
                                  "Third transmission", "Final package"};
    
    for (const auto& msg : message_set)
    {
        auto text_unit = make_unique<simple_text_buffer>(msg);
        sync_handler.transmit_buffer(move(text_unit));
    }
    
    sleep_for(milliseconds(1000));
    sync_handler.signal_completion();
}

void data_processor(synchronization_manager& sync_handler)
{
    while (true)
    {
        auto processed_unit = sync_handler.acquire_buffer();
        
        if (!processed_unit)
        {
            cout << "---------------\nPROCESSING TERMINATED\n";
            break;
        }
    }
}

int main()
{
    synchronization_manager sync_unit;
    
    thread generator(data_generator, ref(sync_unit));
    thread processor(data_processor, ref(sync_unit));
    
    generator.join();
    processor.join();
    
    return 0;
}
