//using NHibernateInAction.CaveatEmptor.Test.Utility;
using NUnit.Framework;

namespace Inference.Tests.Utility
{
    /// <summary>
    /// Utility class for when you just want to insert some test data.
    /// </summary>
    [TestFixture /*, Explicit */ ]
    public class TestDataFixture : TestFixtureWithSampleDataBase
    {
        [Test]
        public void CanInitDataWithoutProblems()
        {
            InitData();
        }
    }
}
