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

namespace Inspirel.YAMI
{
    /// <summary>
    /// Library version information.
    /// </summary>
    public class Version
    {
        private const int major = 1;
        private const int minor = 10;
        private const int build = 1;

        /// <summary>
        /// Library verion name.
        /// </summary>
        public static string VersionName
        {
            get
            {
                return string.Format("{0}.{1}.{2}", major, minor, build);
            }
        }

        /// <summary>
        /// Library version number (X * 10000 + Y * 100 + Z).
        /// </summary>
        public static int VersionNumber
        {
            get
            {
                return major * 10000 + minor * 100 + build;
            }
        }
    }
}
