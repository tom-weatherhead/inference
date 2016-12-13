//#define ENABLE_LOGGING

using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Remoting.Messaging;    // For CallContext
using System.Text;
using NHibernate;
using NHibernate.Cfg;
using SchemaExport = NHibernate.Tool.hbm2ddl.SchemaExport;
using SchemaUpdate = NHibernate.Tool.hbm2ddl.SchemaUpdate;
//using Inference.Exceptions;

namespace Inference.Persistence
{
    public static class NHibernateHelper
    {
#if OLD_20100219
        private static readonly string uniqueName = Guid.NewGuid().ToString();
        //private static readonly string nameSession = uniqueName + "_NHibernate.ISession";
        private static readonly string nameTransaction = uniqueName + "_NHibernate.ITransaction";
        //private static readonly string nameInterceptor = uniqueName + "_NHibernate.IInterceptor";
#endif

        /*
        public static Configuration Configuration
        {
            get
            {
                return NHibernateHelper_ThreadAndHttpContext.Configuration;
            }
        }
         */

        public static SchemaExport GetSchemaExportObject()
        {
            return NHibernateHelper_ThreadAndHttpContext.GetSchemaExportObject();
        }

        public static SchemaUpdate GetSchemaUpdateObject()
        {
            return NHibernateHelper_ThreadAndHttpContext.GetSchemaUpdateObject();
        }

        /*
        public static ISession GetSession()
        {
            return NHibernateHelper_ThreadAndHttpContext.GetSession();
        }
         */

        public static ISession Session
        {
            get
            {
                return NHibernateHelper_ThreadAndHttpContext.GetSession();
                /*
                ISession s = (ISession)CallContext.GetData(nameSession);
                try
                {
                    if (s == null)
                    {
                        log.Debug("Opening new Session for this context.");
                        if (Interceptor != null)
                        {
                            log.Debug("Using interceptor: " + Interceptor.GetType());
                            s = SessionFactory.OpenSession(Interceptor);
                        }
                        else
                        {
                            s = SessionFactory.OpenSession();
                        }
                        CallContext.SetData(nameSession, s);
                    }
                }
                catch (HibernateException ex)
                {
                    throw new InfrastructureException(ex);
                }
                return s;
                 */
            }
        }

        public static void CloseSession()
        {
            NHibernateHelper_ThreadAndHttpContext.CloseSession();
            /*
            try
            {
                ISession s = (ISession)CallContext.GetData(nameSession);
                CallContext.SetData(nameSession, null);
                if (s != null && s.IsOpen)
                {
                    log.Debug("Closing Session of this context.");
                    s.Close();
                }
                else
                    log.Warn("Useless call of CloseSession().");
            }
            catch (HibernateException ex)
            {
                throw new InfrastructureException(ex);
            }
             */
        }

        /// <summary>Start a new database transaction.</summary>
        public static void BeginTransaction()
        {
#if OLD_20100219
            ITransaction tx = (ITransaction)CallContext.GetData(nameTransaction);

            try
            {

                if (tx == null)
                {
#if ENABLE_LOGGING
                    log.Debug("Starting new database transaction in this context.");
#endif
                    tx = Session.BeginTransaction();
                    CallContext.SetData(nameTransaction, tx);
                }
            }
            catch (HibernateException ex)
            {
                throw new InfrastructureException(ex);
            }
#else
            NHibernateHelper_ThreadAndHttpContext.BeginTransaction();
#endif
        }

        /// <summary>Commit the database transaction.</summary>
        public static void CommitTransaction()
        {
#if OLD_20100219
            ITransaction tx = (ITransaction)CallContext.GetData(nameTransaction);

            try
            {

                if (tx != null && !tx.WasCommitted && !tx.WasRolledBack)
                {
#if ENABLE_LOGGING
                    log.Debug("Committing database transaction of this context.");
#endif
                    tx.Commit();
                }

                CallContext.SetData(nameTransaction, null);
            }
            catch (HibernateException ex)
            {
                RollbackTransaction();
                throw new InfrastructureException(ex);
            }
#else
            NHibernateHelper_ThreadAndHttpContext.CommitTransaction();
#endif
        }

        /// <summary>Roll back the database transaction.</summary>
        public static void RollbackTransaction()
        {
#if OLD_20100219
            ITransaction tx = (ITransaction)CallContext.GetData(nameTransaction);

            try
            {
                CallContext.SetData(nameTransaction, null);

                if (tx != null && !tx.WasCommitted && !tx.WasRolledBack)
                {
#if ENABLE_LOGGING
                    log.Debug("Trying to rollback database transaction of this context.");
#endif
                    tx.Rollback();
                }
            }
            catch (HibernateException ex)
            {
                throw new InfrastructureException(ex);
            }
            finally
            {
                CloseSession();
            }
#else
            NHibernateHelper_ThreadAndHttpContext.RollbackTransaction();
#endif
        }
    }
}
