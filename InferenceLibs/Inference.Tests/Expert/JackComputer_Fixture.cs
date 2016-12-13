using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using Inference.Expert;
using NUnit.Framework;

namespace Inference.Tests.Expert
{
    class FloppyAlignmentQuery : ExpertQueryBase
    {
        public FloppyAlignmentQuery()
        {
            Answers["Is the computer plugged in?"] = true;
            Answers["Is power available at the socket?"] = true;
            Answers["Is the computer switched on?"] = true;
            Answers["Is the monitor plugged in?"] = true;
            Answers["Is the monitor switched on?"] = true;
            Answers["Is the computer connected to its monitor?"] = true;
            Answers["Does the monitor display a picture?"] = true;
            Answers["Is the floppy drive's fuse blown?"] = false;
            Answers["Is there a disk in the computer's floppy drive?"] = true;
            Answers["Is the disk in the computer's floppy drive formatted?"] = true;
            Answers["Is the disk in the computer's floppy drive known to contain files?"] = true;
            Answers["Is data successfully read from the computer's floppy drive?"] = false;
        }
    }

    [TestFixture]
    public class JackComputer_Fixture
    {
        [Test]
        public void FloppyAlignmentTest()
        {
            IExpertQuery queryInterface = new FloppyAlignmentQuery();
            ModifiedAStarAlgorithm expertSystem = new ModifiedAStarAlgorithm(DomainSelector.JackComputer, queryInterface);
            ModifiedAStarState goalState = expertSystem.Search();

            Assert.IsNotNull(goalState);
            Assert.AreEqual("The computer's floppy drive is out of alignment.", goalState.diagnosis);
        }
    }
}
