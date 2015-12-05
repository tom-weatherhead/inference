using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Expert;
using NUnit.Framework;

namespace Inference.Tests.Expert
{
    class DeadBatteryQuery : ExpertQueryBase
    {
        public DeadBatteryQuery()
        {
            Answers["Does the car's radio work?"] = false;
        }
    }

    class AnimalDiceQuery : ExpertQueryBase
    {
        public AnimalDiceQuery()
        {
            Answers["Does the car's radio work?"] = true;
            Answers["Is there a bullet hole in the car?"] = false;
            Answers["Is there animal fur on the car's driver's seat?"] = true;
            Answers["Are there claw marks on the car's steering wheel?"] = true;
            Answers["Were dice found in the car?"] = true;
            Answers["Were playing cards found in the car?"] = false;
        }
    }

    [TestFixture]
    public class JackAuto_Fixture
    {
        [Test]
        public void DeadBatteryTest()
        {
            IExpertQuery queryInterface = new DeadBatteryQuery();
            ModifiedAStarAlgorithm expertSystem = new ModifiedAStarAlgorithm(DomainSelector.JackAutomotive, queryInterface);
            ModifiedAStarState goalState = expertSystem.Search();

            Assert.IsNotNull(goalState);
            Assert.AreEqual("The car's battery is dead.", goalState.diagnosis);
        }

        [Test]
        public void AnimalDiceTest()
        {
            IExpertQuery queryInterface = new AnimalDiceQuery();
            ModifiedAStarAlgorithm expertSystem = new ModifiedAStarAlgorithm(DomainSelector.JackAutomotive, queryInterface);
            ModifiedAStarState goalState = expertSystem.Search();

            Assert.IsNotNull(goalState);
            Assert.AreEqual("An animal drove the car into the ground during a cross-country gambling binge.", goalState.diagnosis);
        }
    }
}
