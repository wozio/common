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
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;

public abstract class Listener {

    public static class ListeningResult {

        public ListeningResult(Channel ch) {
            this.channel = ch;
        }

        public ListeningResult(String target, ByteBuffer buffer) {
            this.target = target;
            this.buffer = buffer;
        }

        // non-null if the result of accept is a new channel
        public Channel channel;

        // non-null if the result is a frame ready to inject
        public String target;
        public ByteBuffer buffer;
    }

    private final SelectableChannel channel;
    private final String resolvedTarget;
    protected SelectionKey lastUsedSelectionKey;

    protected final LogCallback logCallback;
    protected final LogCallback.LogLevel logLevel;

    Listener(SelectableChannel channel,
            String resolvedTarget,
            LogCallback logCallback, LogCallback.LogLevel logLevel) {

        this.channel = channel;
        this.resolvedTarget = resolvedTarget;

        this.logCallback = logCallback;
        this.logLevel = logLevel;
    }

    public void close() {
        try {
            if (lastUsedSelectionKey != null) {
                lastUsedSelectionKey.cancel();
            }
            channel.close();
        } catch (IOException ex) {
            // ignore
        }
    }

    public String getResolvedTarget() {
        return resolvedTarget;
    }

    // to be defined by concrete listener
    abstract ListeningResult accept() throws IOException;

    // to be defined by concrete listener
    abstract SelectionKey registerForSelection(Selector selector);
}
