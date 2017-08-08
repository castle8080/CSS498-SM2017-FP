using System;
using System.Linq;
using System.Net.Http;
using System.Threading;
using System.Collections.Generic;

using static csctrl.Control;

namespace csctrl
{

    class Program
    {

        static void Main2(string[] args)
        {
            int[] numbers = { 1, 2 };
            ForEach(numbers, x => {
                Console.WriteLine($"Have x -> {x}!");
            });

            var hc = new HttpClient();
            var content = Retry(3, () => {
                return hc.GetStringAsync("https://www.google.com/#q=folds").Result;
            });
            Console.WriteLine(content);


            var fileContent = ReadFile("Control.cs", (tr) => tr.ReadToEnd());
            Console.WriteLine(fileContent);
        }

        static void Main(string[] args) {
            /*
            ILockManager manager = new RedisLockManager("localhost:6379");
            Console.WriteLine($"[{DateTime.Now}] Starting program!");
            manager.WithLock("master", () => {
                Console.WriteLine($"[{DateTime.Now}] Have lock!");
                Thread.Sleep(5000);
                Console.WriteLine($"[{DateTime.Now}] Done with lock!");
                return true;
            });
            */


            var queue = new Queue<int>();
            queue.Enqueue(1);
            queue.Enqueue(10);
            while (queue.Count > 0) {
                Console.WriteLine("Item: " + queue.Dequeue());
            }
        }

    }
}
