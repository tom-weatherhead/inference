//#define InferenceWeb

using System;
using System.Collections.Generic;
using System.Threading;
using System.Web;
using NHibernate;
using NHibernate.Cfg;
using SchemaExport = NHibernate.Tool.hbm2ddl.SchemaExport;
using SchemaUpdate = NHibernate.Tool.hbm2ddl.SchemaUpdate;
//using Inference.Exceptions;

// TODO: Create an "ISessionStorage" interface with properties Session and Transaction (get and set)
// Then implement it for ThreadContext, HttpContext, CallContext, etc.
// Make NHibernateHelper select a SessionStorage and use it...
// Cf. Sample - Wiki_AndrewMayorovAspNet.zip

/* In Web.config
<httpModules>
	<add name="NHibernateHelper_ThreadAndHttpContext"
		type="NHibernateInAction.CaveatEmptor.Persistence.HttpRequestModule, NHibernateInAction.CaveatEmptor" />
 * Or, in our case:
	<add name="NHibernateHelper_ThreadAndHttpContext"
		type="Alexandria.Persistence.HttpRequestModule, Alexandria" />
</httpModules>
*/

/* HttpRequestModule.cs
public class HttpRequestModule : IHttpModule
{
	public String ModuleName
	{
		get { return "NHibernateHelper_ThreadAndHttpContext"; }
	}
 
	public void Init(HttpApplication application)
	{
		application.BeginRequest += new EventHandler(this.Application_BeginRequest);
		application.EndRequest += new EventHandler(this.Application_EndRequest);
		application.Error += new EventHandler(this.Application_Error);
	}

	private void Application_BeginRequest(object source, EventArgs e)
	{
		// Note: This is useless (because the session will be eventually created when needed)
		// And it may be a waste of ressources because it might not be used at all
		NHibernateHelper_ThreadAndHttpContext.GetSession();
	}

	private void Application_EndRequest(object source, EventArgs e)
	{
		NHibernateHelper_ThreadAndHttpContext.CloseSession();
	}

	private void Application_Error(object sender, EventArgs e)
	{
		// TODO: Useful to add?
		// Most of the time, it should be possible to catch the error and rollback
		NHibernateHelper_ThreadAndHttpContext.RollbackAndCloseSession();
	}

	public void Dispose() {}
}*/

namespace Inference.Persistence
{
    interface IContextCache<T>
    {
        T Get();
        void Store(T obj);
        void Remove();
    }

    class ThreadCache<T> : IContextCache<T> where T : class
    {
        private readonly Dictionary<string, T> threadDictionary = new Dictionary<string, T>();

        public ThreadCache()
        {
        }

        public T Get()
        {
            T obj = null;   // ISession session = null;
            Thread threadCurrent = Thread.CurrentThread;

            if (threadCurrent.Name == null)
            {
                threadCurrent.Name = Guid.NewGuid().ToString();
            }
            else if (threadDictionary.ContainsKey(threadCurrent.Name))
            {
                /*
                object threadSession = _threadSessions[threadCurrent.Name];

                if (threadSession != null)
                {
                    session = (ISession)threadSession;
                }
                 */
                obj = threadDictionary[threadCurrent.Name];
            }

            return obj; // session;
        }

        public void Store(T obj)
        {
            Thread threadCurrent = Thread.CurrentThread;

            if (threadCurrent.Name == null)
            {
                threadCurrent.Name = Guid.NewGuid().ToString();
            }

            if (threadDictionary.ContainsKey(threadCurrent.Name))
            {
                threadDictionary[threadCurrent.Name] = obj;
            }
            else
            {
                threadDictionary.Add(threadCurrent.Name, obj);
            }
        }

        public void Remove()
        {
            Thread threadCurrent = Thread.CurrentThread;

            if (threadCurrent.Name != null && threadDictionary.ContainsKey(threadCurrent.Name))
            {
                threadDictionary.Remove(threadCurrent.Name);
            }
        }
    }

#if InferenceWeb
    class HttpContextCache<T> : IContextCache<T> where T : class
    {
        private readonly string key;

        public HttpContextCache(string key)
        {
            this.key = key;
        }

        public T Get()
        {

            if (HttpContext.Current.Items.Contains(key))
            {
                return (T)HttpContext.Current.Items[key];
            }

            return null;
        }

        public void Store(T obj)
        {

            if (HttpContext.Current.Items.Contains(key))
            {
                HttpContext.Current.Items[key] = obj;
            }
            else
            {
                HttpContext.Current.Items.Add(key, obj);
            }
        }

        public void Remove()
        {

            if (HttpContext.Current.Items.Contains(key))
            {
                HttpContext.Current.Items.Remove(key);
            }
        }
    }
#endif

    static class ContextCacheFactory
    {
        public static IContextCache<T> CreateContextCache<T>(string key) where T : class
        {
#if InferenceWeb
            if (HttpContext.Current != null)
            {
                return new HttpContextCache<T>(key);
            }
#endif

            return new ThreadCache<T>();
        }
    }

    /// <summary>
	/// Provides "Thread Context" and HttpContext to manage sessions.
	/// </summary>
	public static class NHibernateHelper_ThreadAndHttpContext
	{
		#region private static variables

        private static readonly Configuration _configuration;
		private static readonly ISessionFactory _sessionFactory;
#if OLD_20100219
		private static Hashtable _threadSessions = new Hashtable();  
		private static readonly bool isHttpContextAvailable = (HttpContext.Current != null);
#endif
		private const string _httpContextSessionKey = "NHibernate.ISession";
        private const string _httpContextTransactionKey = "NHibernate.ITransaction";
        private static readonly IContextCache<ISession> _sessionCache = ContextCacheFactory.CreateContextCache<ISession>(_httpContextSessionKey);
        private static readonly IContextCache<ITransaction> _transactionCache = ContextCacheFactory.CreateContextCache<ITransaction>(_httpContextTransactionKey);

		#endregion
 

		#region constructors
 
        /*
		private NHibernateHelper_ThreadAndHttpContext()
        {
        }
         */
 
		static NHibernateHelper_ThreadAndHttpContext()
		{
			try 
			{
                /*
            var cfg = new Configuration();

            cfg.Configure();
            cfg.AddAssembly(typeof (Product).Assembly);

            const bool script = false;
            const bool export = true;
            const bool justDrop = false;
            //const bool format = false;

            //new SchemaExport(cfg).Execute(false, true, false, false);
            new SchemaExport(cfg).Execute(script, export, justDrop);
                 */

                _configuration = new Configuration();
                _configuration.Configure();
                _configuration.AddAssembly(typeof(Inference.Domain.Clause).Assembly);
				_sessionFactory = _configuration.BuildSessionFactory();  
			} 
			catch (Exception ex) 
			{
				throw new Exception(System.Reflection.MethodInfo.GetCurrentMethod().Name + " error: ", ex);
			}
		}
 
		#endregion
 

		# region private static methods
 
		/// <summary>
		/// gets the current session   
		/// </summary>
		private static ISession GetCurrentSession()
		{
#if OLD_20100219
			ISession session = null;

            if (!isHttpContextAvailable)
            {
                session = GetCurrentThreadSession();
            }
            else
            {
                session = GetCurrentHttpContextSession();
            }

			return session;
#else
            return _sessionCache.Get();
#endif
		}
 
		/// <summary>
		/// sets the current session   
		/// </summary>
		private static void StoreCurrentSession(ISession session)
		{
#if OLD_20100219

            if (!isHttpContextAvailable)
            {
                StoreCurrentThreadSession(session);
            }
            else
            {
                StoreCurrentHttpContextSession(session);
            }
#else
            _sessionCache.Store(session);
#endif
		}
 
		/// <summary>
		/// sets the current session   
		/// </summary>
		private static void RemoveCurrentSession()
		{
#if OLD_20100219

            if (!isHttpContextAvailable)
            {
                RemoveCurrentThreadSession();
            }
            else
            {
                RemoveCurrentHttpContextSession();
            }
#else
            _sessionCache.Remove();
#endif
		}

        private static ITransaction GetCurrentTransaction()
        {
            return _transactionCache.Get();
        }

        private static void StoreCurrentTransaction(ITransaction transaction)
        {
            _transactionCache.Store(transaction);

            if (transaction != null && GetCurrentTransaction() == null)
            {
                throw new Exception("NHibernateHelper_ThreadAndHttpContext.StoreCurrentTransaction() failed.");
            }
        }

        private static void RemoveCurrentTransaction()
        {
            _transactionCache.Remove();
        }

		# endregion
 

#if OLD_20100219
		#region private static methods - HttpContext related
 
		/// <summary>
		/// gets the session for the current thread
		/// </summary>
		private static ISession GetCurrentHttpContextSession()
		{

            if (HttpContext.Current.Items.Contains(_httpContextSessionKey))
            {
                return (ISession)HttpContext.Current.Items[_httpContextSessionKey];
            }

			return null;
		}
 
		private static void StoreCurrentHttpContextSession(ISession session)
		{

            if (HttpContext.Current.Items.Contains(_httpContextSessionKey))
            {
                HttpContext.Current.Items[_httpContextSessionKey] = session;
            }
            else
            {
                HttpContext.Current.Items.Add(_httpContextSessionKey, session);
            }
		}
 
		/// <summary>
		/// remove the session for the currennt HttpContext
		/// </summary>
		private static void RemoveCurrentHttpContextSession()
		{

            if (HttpContext.Current.Items.Contains(_httpContextSessionKey))
			{
                HttpContext.Current.Items.Remove(_httpContextSessionKey);
            }
		}
 
		#endregion


		#region private static methods - ThreadContext related
 
		/// <summary>
		/// gets the session for the current thread
		/// </summary>
		private static ISession GetCurrentThreadSession()
		{
			ISession session = null;
			Thread threadCurrent = Thread.CurrentThread;

            if (threadCurrent.Name == null)
            {
                threadCurrent.Name = Guid.NewGuid().ToString();
            }
            else
            {
                object threadSession = _threadSessions[threadCurrent.Name];

                if (threadSession != null)
                {
                    session = (ISession)threadSession;
                }
            }

			return session;
		}
 
		private static void StoreCurrentThreadSession(ISession session)
		{

            if (_threadSessions.Contains(Thread.CurrentThread.Name))
            {
                _threadSessions[Thread.CurrentThread.Name] = session;
            }
            else
            {
                _threadSessions.Add(Thread.CurrentThread.Name, session);
            }
		}
 
		private static void RemoveCurrentThreadSession()
		{

            if (_threadSessions.Contains(Thread.CurrentThread.Name))
            {
                _threadSessions.Remove(Thread.CurrentThread.Name);
            }
		}
 
		#endregion
#endif


        #region public static methods

        /// <summary>
		/// gets a session (creates new one if none available) 
		/// </summary>
		/// <returns>a session</returns>
		public static ISession GetSession()
		{
            // ThAW 2011/07/23 : Test
            //return _sessionFactory.OpenSession();
            
			ISession session = GetCurrentSession();

            if (session == null)
            {
                session = _sessionFactory.OpenSession();
            }

            if (!session.IsConnected)
            {
                session.Reconnect();
            }

			StoreCurrentSession(session);
			return session;
		}
 
		/// <summary>
		/// closes the current session
		/// </summary>
		public static void CloseSession()
		{
			ISession session = GetCurrentSession();

            if (session == null)
            {
                return;
            }

            if (!session.IsConnected)
            {
                session.Reconnect();
            }

			session.Flush();
			session.Close();
			RemoveCurrentSession();
		}
 
		/// <summary>
		/// disconnects the current session (if not in active transaction)
		/// </summary>
		public static void DisconnectSession()
		{
			ISession session = GetCurrentSession();

            if (session == null)
            {
                return;
            }

            if (!session.IsConnected)
            {
                return;
            }

			if (session.Transaction == null || session.Transaction.WasCommitted || session.Transaction.WasRolledBack)
            {
				session.Disconnect();  
			}
		}

        /// <summary>Start a new database transaction.</summary>
        public static void BeginTransaction()
        {
            ITransaction tx = GetCurrentTransaction();  // (ITransaction)CallContext.GetData(nameTransaction);

            try
            {

                if (tx == null)
                {
#if ENABLE_LOGGING
                    log.Debug("Starting new database transaction in this context.");
#endif
                    tx = GetSession().BeginTransaction();
                    StoreCurrentTransaction(tx);    // CallContext.SetData(nameTransaction, tx);
                }
            }
            catch (HibernateException /* ex */ )
            {
                throw;  // new InfrastructureException(ex);
            }
        }

        /// <summary>Commit the database transaction.</summary>
        public static void CommitTransaction()
        {
            ITransaction tx = GetCurrentTransaction();  //(ITransaction)CallContext.GetData(nameTransaction);

            try
            {

                if (tx != null && !tx.WasCommitted && !tx.WasRolledBack)
                {
#if ENABLE_LOGGING
                    log.Debug("Committing database transaction of this context.");
#endif
                    tx.Commit();
                }

                RemoveCurrentTransaction(); // CallContext.SetData(nameTransaction, null);
            }
            catch (HibernateException /* ex */ )
            {
                RollbackTransaction();
                throw;  // new InfrastructureException(ex);
            }
        }

        /// <summary>Roll back the database transaction.</summary>
        public static void RollbackTransaction()
        {
            ITransaction tx = GetCurrentTransaction();  //(ITransaction)CallContext.GetData(nameTransaction);

            try
            {
                RemoveCurrentTransaction(); // CallContext.SetData(nameTransaction, null);

                if (tx != null && !tx.WasCommitted && !tx.WasRolledBack)
                {
#if ENABLE_LOGGING
                    log.Debug("Trying to rollback database transaction of this context.");
#endif
                    tx.Rollback();
                }
            }
            catch (HibernateException /* ex */ )
            {
                throw;  // new InfrastructureException(ex);
            }
            finally
            {
                CloseSession();
            }
        }

        public static SchemaExport GetSchemaExportObject()
        {
            return new SchemaExport(_configuration);
        }

        public static SchemaUpdate GetSchemaUpdateObject()
        {
            return new SchemaUpdate(_configuration);
        }

        #endregion


        /*
        #region public static properties

        public static Configuration Configuration   // ThAW
        {
            get
            {
                return _configuration;
            }
        }

        #endregion
         */
    }
}
