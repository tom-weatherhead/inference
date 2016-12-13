using System;
//using NUnit.Framework;
//using SchemaExport = NHibernate.Tool.hbm2ddl.SchemaExport;
//using NHibernateHelper = NHibernateInAction.CaveatEmptor.Persistence.NHibernateHelper;
//using Iesi.Collections.Generic;
using NHibernateHelperLib.Persistence;
//using Inference.Persistence;

namespace Inference.Tests.Utility
{
    //[TestFixture]
    public abstract class TestFixtureBase 
    {
        protected void CreateDatabase()
        {
            /*
            var ddlExport = NHibernateHelper.GetSchemaExportObject();  // new SchemaExport(NHibernateHelper.Configuration);
            
            //if we want to actually look at the DDL generated, we can do this by passing true as the first parameter to Create().
            //ddlExport.SetOutputFile("c:\\Inference.sql");
            
            //ddlExport.Create(false, true);
            ddlExport.Create(true, true);
             */
            NHibernateHelper.CreateDatabase();
        }

        protected void DropDatabase()
        {
            /*
            //closing the session makes sure it's not using the database
            //thus allowing the drop statements to run quickly
            NHibernateHelper.CommitTransaction();
            NHibernateHelper.CloseSession();

            var ddlExport = NHibernateHelper.GetSchemaExportObject();  //new SchemaExport(NHibernateHelper.Configuration);

            //ddlExport.Drop(false, true);
            ddlExport.Drop(true, true);
             */
            NHibernateHelper.DropDatabase();
        }
    }
}
