
namespace csctrl {
    using ServiceStack.Redis;
    using System;

    public interface ILockManager {
        T WithLock<T>(string lockName, Func<T> body);
    }

    public class RedisLockManager : ILockManager {

        private RedisManagerPool pool;

        public RedisLockManager(string redisConnection) {
            pool = new RedisManagerPool(redisConnection);
        }

        public T WithLock<T>(string lockName, Func<T> body) {
            using (var client = pool.GetClient()) {
                using (var lck = client.AcquireLock(lockName)) {
                    return body();
                }
            }
        }
    }

}
