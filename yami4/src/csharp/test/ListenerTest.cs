// Copyright Paweł Kierski 2010, 2015.
// This file is part of YAMI4.
//
// YAMI4 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// YAMI4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

using NUnit.Framework;
namespace Inspirel.YAMI
{
    [TestFixture]
    public class ListenerTest
    {
        private Agent agent;

        [SetUp]
        public virtual void setUp()
        {
            agent = new Agent();
        }

        [TearDown]
        public virtual void tearDown()
        {
            agent.Close();
        }

/// 
/// <summary> test for resolution of local address </summary>
/// 
        [Test]
        public void testForLocalResolution()
        {
            string localHostName = System.Net.Dns.GetHostName();

            string resolvedTarget = agent.AddListener("tcp://*:*");
            Assert.IsTrue(resolvedTarget.StartsWith(
                "tcp://" + localHostName + ":"));

            resolvedTarget = agent.AddListener("udp://*:*");
            Assert.IsTrue(resolvedTarget.StartsWith(
                "udp://" + localHostName + ":"));
        }
    }
}