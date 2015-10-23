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

using System.Threading;

namespace Inspirel.YAMI.details
{
    internal class WaterFlowManager
    {
        private int highMark;
        private int lowMark;
        private int currentLevel;
        private bool allowFlow;

        public WaterFlowManager(int highMark, int lowMark)
        {
            this.highMark = highMark;
            this.lowMark = lowMark;
            currentLevel = 0;
            allowFlow = true;
        }

        public bool isAllowed()
        {
            lock(this)
            {
                return allowFlow;
            }
        }

        public void waitForPermission()
        {
            lock(this)
            {
                while(allowFlow == false)
                {
                    try
                    {
                        Monitor.Wait(this);
                    }
                    catch(System.Exception)
                    {
                        // ignore, cannot happen
                    }
                }
            }
        }

        public void increase()
        {
            lock(this)
            {
                ++currentLevel;
                if(currentLevel >= highMark)
                {
                    allowFlow = false;
                }
            }
        }

        // returns true if as a result of this call the allowFlow flag
        // is *changed* from false to true
        public bool decrease()
        {
            lock(this)
            {
                --currentLevel;
                if(currentLevel < lowMark)
                {
                    allowFlow = true;
                    Monitor.Pulse(this);
                    return true;
                }

                return false;
            }
        }

        public Agent.OutgoingFlowState getFlowState()
        {
            Agent.OutgoingFlowState state = new Agent.OutgoingFlowState();
            lock(this)
            {
                state.currentLevel = currentLevel;
                state.highWaterMark = highMark;
                state.lowWaterMark = lowMark;
            }

            return state;
        }
    }
}	// end namespace