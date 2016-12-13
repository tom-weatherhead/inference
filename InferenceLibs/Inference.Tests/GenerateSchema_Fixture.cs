using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;
using Inference.Tests.Utility;
//using NHibernate.Cfg;
//using NHibernate.Tool.hbm2ddl;
using NUnit.Framework;

namespace Inference.Tests
{
    [TestFixture]
    public class GenerateSchema_Fixture : TestFixtureBase
    {
        public GenerateSchema_Fixture()
        {
            Inference.Persistence.PersistenceInitializer.Init();
        }

        [Test]
        public void CanGenerateSchema()
        {
            DropDatabase();
            CreateDatabase();
        }
    }
}
