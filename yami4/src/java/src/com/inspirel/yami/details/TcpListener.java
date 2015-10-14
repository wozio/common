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

import com.inspirel.yami.LogCallback;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;

public class TcpListener extends Listener {

    private final ServerSocketChannel channel;
    private final IncomingMessageDispatchCallback
            incomingMessageDispatchCallback;
    private final Options options;

    TcpListener(ServerSocketChannel channel,
            String resolvedTarget,
            IncomingMessageDispatchCallback incomingMessageDispatchCallback,
            Options options,
            LogCallback logCallback, LogCallback.LogLevel logLevel) {

        super(channel, resolvedTarget, logCallback, logLevel);

        this.channel = channel;
        this.incomingMessageDispatchCallback =
                incomingMessageDispatchCallback;
        this.options = options;
    }
    
    @Override
    public SelectionKey registerForSelection(Selector selector) {
        
        final int operations = SelectionKey.OP_ACCEPT;

        try {
            lastUsedSelectionKey = channel.register(selector, operations);
        } catch (ClosedChannelException ex) {
            // ignore, will never happen
        }
        
        return lastUsedSelectionKey;
    }
    
    @Override
    public ListeningResult accept() throws IOException {
        
        SocketChannel acceptedChannel = channel.accept();
        
        NetworkUtils.configureTcpChannel(acceptedChannel, options);
        
        Socket s = acceptedChannel.socket();
        InetAddress address = s.getInetAddress();
        String hostName = address.getHostAddress();
        int port = s.getPort();
        
        String sourceTarget = NetworkUtils.formatTcpTarget(hostName, port);
        
        Channel newChannel = new Channel(acceptedChannel, sourceTarget,
                incomingMessageDispatchCallback, options,
                logCallback, logLevel);
        
        return new ListeningResult(newChannel);
    }
}
