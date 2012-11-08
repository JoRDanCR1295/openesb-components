/*
 * 
 * =======================================================================
 * Copyright (c) 2002-2005 Axion Development Team.  All rights reserved.
 *  
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * 1. Redistributions of source code must retain the above 
 *    copyright notice, this list of conditions and the following 
 *    disclaimer. 
 *   
 * 2. Redistributions in binary form must reproduce the above copyright 
 *    notice, this list of conditions and the following disclaimer in 
 *    the documentation and/or other materials provided with the 
 *    distribution. 
 *   
 * 3. The names "Tigris", "Axion", nor the names of its contributors may 
 *    not be used to endorse or promote products derived from this 
 *    software without specific prior written permission. 
 *  
 * 4. Products derived from this software may not be called "Axion", nor 
 *    may "Tigris" or "Axion" appear in their names without specific prior
 *    written permission.
 *   
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * =======================================================================
 */

package org.axiondb.engine.rowiterators;

import java.util.NoSuchElementException;

import org.axiondb.AxionException;
import org.axiondb.Row;
import org.axiondb.RowIterator;

/**
 * A {@link RowIterator}over a single {@link Row}.
 * 
 * @version  
 * @author Rodney Waldhoff
 * @author Ahimanikya Satapathy
 */
public class SingleRowIterator implements RowIterator {

    public SingleRowIterator(Row row) {
        _row = row;
    }

    public void add(Row row) {
        throw new UnsupportedOperationException();
    }

    public Row current() {
        if (_currentSet) {
            return _row;
        }
        throw new NoSuchElementException();
    }

    public int currentIndex() {
        if (_currentSet) {
            return 0;
        }
        return -1;
    }

    public Row first() {
        _currentSet = true;
        _before = true;
        return _row;
    }

    public boolean hasCurrent() {
        return _currentSet;
    }

    public boolean hasNext() {
        return _before;
    }

    public boolean hasPrevious() {
        return !_before;
    }

    public boolean isEmpty() {
        return false;
    }

    public Row last() {
        _currentSet = true;
        _before = false;
        return _row;
    }

    public Row next() {
        if (_before) {
            return last();
        }
        throw new NoSuchElementException();
    }

    public int next(int count) throws AxionException {
        throw new UnsupportedOperationException();
    }

    public int nextIndex() {
        if (_before) {
            return 0;
        }
        return 1;
    }

    public Row peekNext() {
        if (_before) {
            return _row;
        }
        throw new NoSuchElementException();
    }

    public Row peekPrevious() {
        if (!_before) {
            return _row;
        }
        throw new NoSuchElementException();
    }

    public Row previous() {
        if (!_before) {
            return first();
        }
        throw new NoSuchElementException();
    }

    public int previous(int count) throws AxionException {
        throw new UnsupportedOperationException();
    }

    public int previousIndex() {
        if (_before) {
            return -1;
        }
        return 0;
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }

    public void reset() {
        _currentSet = false;
        _before = true;
    }

    public void set(Row row) {
        throw new UnsupportedOperationException();
    }

    public int size() throws AxionException {
        return 1;
    }

    public String toString() {
        return "SingleRowIterator(" + _row + ")";
    }

    private boolean _before = true;
    private boolean _currentSet = false;
    private Row _row = null;
}


