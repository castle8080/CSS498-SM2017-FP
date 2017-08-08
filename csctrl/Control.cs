using System;
using System.IO;

namespace csctrl
{

    public static class Control {
        public static void ForEach<T>(T[] array, Action<T> action) {
            for (int i = 0; i < array.Length; i++)
                action(array[i]);
        }

        public static T ReadFile<T>(String filePath, Func<TextReader, T> processor) {
            var fs = new StreamReader(new FileStream(filePath, FileMode.Open));
            try {
                return processor(fs);
            }
            finally {
                fs.Dispose();
            }
        }

        public static T Retry<T>(int maxTries, Func<T> operation) {
            int tryCount = 0;
            while (true) {
                try {
                    tryCount++;
                    return operation();
                }
                catch (Exception e) {
                    if (tryCount >= maxTries)
                        throw e;
                }
            } 
        }

    }
}