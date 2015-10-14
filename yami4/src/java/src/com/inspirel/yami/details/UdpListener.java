// Copyright Maciej Sobczak 2008-2015.
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

package com.inspirel.yami.details;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;

import com.inspirel.yami.LogCallback;

public class UdpListener extends Listener {

    private final DatagramChannel channel;
    private final Options options;

    /**
     * @param incomingMessageDispatchCallback not used here
     */
    UdpListener(DatagramChannel channel,
            String resolvedTarget,
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            Options options,
            LogCallback logCallback, LogCallback.LogLevel logLevel) {

        super(channel, resolvedTarget, logCallback, logLevel);

        this.channel = channel;
        this.options = options;
    }

    @Override
    public SelectionKey registerForSelection(Selector selector) {

        final int operations = SelectionKey.OP_READ;

        try {
            lastUsedSelectionKey = channel.register(selector, operations);
        } catch (ClosedChannelException ex) {
            // ignore, will never happen
        }

        return lastUsedSelectionKey;
    }

    @Override
    public ListeningResult accept() throws IOException {

        // the concept of "accepting" from a UDP listener is a bit artificial
        // and relates to the fact of receiving any datagram
        // this datagram needs to be injected into proper channel

        ByteBuffer buffer = ByteBuffer.allocate(options.udpFrameSize);
        SocketAddress address = channel.receive(buffer);

        if (address instanceof InetSocketAddress) {
            InetSocketAddress inetAddress = (InetSocketAddress) address;

            String clientTarget = NetworkUtils.udpPrefix +
                    inetAddress.getHostName() + ":" +
                    inetAddress.getPort();

            return new ListeningResult(clientTarget, buffer);
            
        } else {
            // ignore messages from non-inet addresses
            // (in practical terms they should never arrive)

            return null;
        }
    }
}
