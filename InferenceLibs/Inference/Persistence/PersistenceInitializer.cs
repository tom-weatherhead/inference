using System;
using System.Collections.Generic;
//using System.Linq;
using System.Reflection;
//using System.Text;
using NHibernateHelperLib.Persistence;

namespace Inference.Persistence
{
    public static class PersistenceInitializer
    {
        //static PersistenceInitializer()
        public static void Init()
        {
            var assemblyList = new List<Assembly>() { typeof(Inference.Domain.Clause).Assembly };

            NHibernateHelper.Initialize(assemblyList);
        }
    }
}
