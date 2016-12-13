////////////////////////////////////////////////////////////////////////////////
// NHiberate In Action - Source Code
// Pierre Henri Kuaté
// January 2009
////////////////////////////////////////////////////////////////////////////////

//#define ENABLE_LOGGING

using System;
using System.Collections.Generic;
using System.Reflection;
//using log4net;
using NHibernate;
using NHibernate.Criterion;     // For the Example class, which used to be in NHibernate.Expression
using NHibernateHelperLib.Persistence;

namespace NHibernateHelperLib.DAO
{
    /// <summary>
    /// A generic DAO class for data access for any mapped entity.
    /// </summary>
    /// <typeparam name="T">The type of entity</typeparam>
    public class DAO<T>
    {
#if ENABLE_LOGGING
        private static readonly ILog _log = LogManager.GetLogger(
            MethodBase.GetCurrentMethod().DeclaringType);
#endif

        public DAO()
        {
#if ENABLE_LOGGING
            _log.Debug("Created new DAO");
#endif
            NHibernateHelper.BeginTransaction();
        }

        /// <summary>
        /// Get an entity by ID, with no upgrade lock.
        /// </summary>
        /// <param name="id"></param>
        /// <returns></returns>
        public virtual T GetById(Guid id)
        {
            return GetById(id, false);
        }
        
        /// <summary>
        /// Get an entity by ID.
        /// </summary>
        /// <param name="id">The id of the entity to load.</param>
        /// <param name="lockIt">Specify true if you want an upgrade lock. 
        /// This will db lock the item for update until the current transaction ends. 
        /// It will also do a version check (comparing columns or version no).</param>
        public virtual T GetById(Guid id, bool lockIt)
        {
#if ENABLE_LOGGING
            _log.Debug("Called GetById: " + id);
#endif

            var session = NHibernateHelper.Session;

            /*
            try
            {
             */

                if (lockIt)
                {
                    return session.Get<T>(id, LockMode.Upgrade);
                }
                else
                {
                    return session.Get<T>(id);
                }
                /*
            }
            catch (HibernateException ex)
            {
                throw;  // new InfrastructureException(ex);
            }
             */
        }


        /// <summary>
        /// Method to find all entities.
        /// </summary>
        /// <returns></returns>
        public virtual IList<T> FindAll()
        {
#if ENABLE_LOGGING
            _log.Debug("Called FindAll");
#endif

            /*
            IList<T> entities;

            try
            {
                entities = NHibernateHelper.Session.CreateCriteria(typeof(T)).List<T>();
            }
            catch (HibernateException ex)
            {
                throw;  // new InfrastructureException(ex);
            }

            return entities;
             */
            return NHibernateHelper.Session.CreateCriteria(typeof(T)).List<T>();
        }

        /// <summary>
        /// Find entities matching this the example one given
        /// </summary>
        /// <param name="exampleEntity">Entity with example properties we'd like to match</param>
        public virtual IList<T> FindByExample(T exampleEntity)
        {
#if ENABLE_LOGGING
            _log.Debug("FindByExample: " + exampleEntity);
#endif

            IList<T> entities;

            /*
            try
            {
             */
                var crit = NHibernateHelper.Session.CreateCriteria(typeof(T));

                entities = crit.Add(Example.Create(exampleEntity)).List<T>();
            /*
            }
            catch (HibernateException ex)
            {
                throw;  // new InfrastructureException(ex);
            }
             */

            return entities;
        }


        /// <summary>
        /// Make this transient or persistent entity persistent in the database
        /// </summary>
        /// <param name="entity"></param>
        public virtual void MakePersistent(T entity)
        {
#if ENABLE_LOGGING
            _log.Debug("Called MakePersistent: " + entity);
#endif

            /*
            try
            {
             */
                NHibernateHelper.Session.SaveOrUpdate(entity);
            /*
            }
            catch (HibernateException ex)
            {
                throw;  // new InfrastructureException(ex);
            }
             */
        }


        /// <summary>
        /// Make the entity transient
        /// </summary>
        /// <remarks>
        /// Making something transient means that it doesn't have a corresponding record in the database; 
        /// It is no longer persistent.
        /// </remarks>
        /// <param name="entity">The entity whose database state will be deleted.</param>
        public virtual void MakeTransient(T entity)
        {
#if ENABLE_LOGGING
            _log.Debug("Called MakeTransient: " + entity);
#endif

            /*
            try
            {
             */
                NHibernateHelper.Session.Delete(entity);
            /*
            }
            catch (HibernateException ex)
            {
                throw;  // new InfrastructureException(ex);
            }
             */
        }
    }
}