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

using System;
using System.Collections.Generic;
using System.Net.Sockets;
using System.Net;

namespace Inspirel.YAMI.details
{
    /// <summary>
    /// Selector-like class
    /// </summary>
    /// <example>
    /// Selector selector = new Selector();
    /// 
    /// while(work)
    /// {
    ///     selector.Clear();
    ///     selector.Add(someSocket, Selector.Direction.READ);
    /// 
    ///     // blocking operation, can be interrupted by 
    ///     // <see cref="Wakeup"/>
    ///     selector.Select();
    /// 
    ///     foreach(Socket s in selector.SelectedKeys)
    ///     {
    ///         if(selector.ReadyForRead(s))
    ///         s.Receive(...);
    ///     }
    /// }
    /// 
    /// selector.Close();
    /// </example>
    internal class Selector
    {
        public enum Direction
        {
            NONE = 0,
            READ = 1,
            WRITE = 2,
            ACCEPT = 4
        }

        public Selector()
        {
            readSocket.Bind(new IPEndPoint(IPAddress.Loopback, 0));
            writeSocket.Connect(readSocket.LocalEndPoint);
        }

        public Socket Add(Socket key, Direction direction)
        {
            if(!readKeys.Contains(key)
                && (direction & Direction.READ) != Direction.NONE)
            {
                readKeys.Add(key);
            }

            if(!writeKeys.Contains(key)
                && (direction & Direction.WRITE) != Direction.NONE)
            {
                writeKeys.Add(key);
            }

            if(!acceptKeys.Contains(key)
                && (direction & Direction.ACCEPT) != Direction.NONE)
            {
                acceptKeys.Add(key);
            }

            return key;
        }

        private List<Socket> readKeys = new List<Socket>();
        private List<Socket> writeKeys = new List<Socket>();
        private List<Socket> acceptKeys = new List<Socket>();
        private List<Socket> lastSelectedKeys = new List<Socket>();

        private Socket writeSocket = NetworkUtils.CreateUDPSocket();
        private Socket readSocket = NetworkUtils.CreateUDPSocket();

        private readonly byte[] dummy = new byte[1];

        public IList<Socket> SelectedKeys
        {
            get
            {
                return lastSelectedKeys;
            }
        }

        public void Clear()
        {
            writeKeys.Clear();
            lastSelectedKeys.Clear();
            acceptKeys.Clear();
            readKeys.Clear();
            readKeys.Add(readSocket);
        }

        public void Wakeup()
        {
            writeSocket.Send(dummy);
        }

        public void Select()
        {
            List<Socket> readAndAcceptKeys = new List<Socket>();
            readAndAcceptKeys.AddRange(readKeys);
            readAndAcceptKeys.AddRange(acceptKeys);

            //TODO: check Select() behaviour on Socket.Close() call
            //List<Socket> errorKeys = new List<Socket>();
            //errorKeys.AddRange(readAndAcceptKeys);
            //errorKeys.RemoveAll(delegate(Socket s)
            //{
            //    return writeKeys.Contains(s);
            //});
            //errorKeys.AddRange(writeKeys);

            Socket.Select(readAndAcceptKeys, writeKeys, 
                null /*errorKeys*/, int.MaxValue);

            //if(errorKeys.Count != 0)
            //{
            //    // what should I do?
            //}

            if(readAndAcceptKeys.Contains(readSocket))
            {
                readAndAcceptKeys.Remove(readSocket);
                readSocket.Receive(dummy);
            }

            readKeys.RemoveAll(delegate(Socket s)
            {
                return !readAndAcceptKeys.Contains(s);
            });
            acceptKeys.RemoveAll(delegate(Socket s)
            {
                return !readAndAcceptKeys.Contains(s);
            });

            lastSelectedKeys.AddRange(readAndAcceptKeys);
            foreach(Socket s in writeKeys)
                if(!lastSelectedKeys.Contains(s))
                    lastSelectedKeys.Add(s);
        }

        public bool ReadyForRead(Socket s)
        {
            return readKeys.Contains(s);
        }

        public bool ReadyForWrite(Socket s)
        {
            return writeKeys.Contains(s);
        }

        public bool ReadyForAccept(Socket s)
        {
            return acceptKeys.Contains(s);
        }

        public void Close()
        {
            if(readSocket != null)
                readSocket.Close();

            if(writeSocket != null)
                writeSocket.Close();
        }
    }
}
