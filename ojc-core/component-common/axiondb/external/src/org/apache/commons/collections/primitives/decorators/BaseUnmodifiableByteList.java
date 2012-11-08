/*
 * $Header: /cvs/open-jbi-components/ojc-core/component-common/axiondb/external/src/org/apache/commons/collections/primitives/decorators/BaseUnmodifiableByteList.java,v 1.1 2007/12/11 14:09:21 jawed Exp $
 * ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.commons.collections.primitives.decorators;

import org.apache.commons.collections.primitives.ByteCollection;
import org.apache.commons.collections.primitives.ByteIterator;
import org.apache.commons.collections.primitives.ByteList;
import org.apache.commons.collections.primitives.ByteListIterator;

/**
 * 
 * @since Commons Primitives 1.0
 * @version $Revision: 1.1 $ $Date: 2007/12/11 14:09:21 $
 * 
 * @author Rodney Waldhoff 
 */
abstract class BaseUnmodifiableByteList extends BaseProxyByteList {

    public final void add(int index, byte element) {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }

    public final boolean addAll(int index, ByteCollection collection) {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }

    public final byte removeElementAt(int index) {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }

    public final byte set(int index, byte element) {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }

    public final boolean add(byte element) {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }

    public final boolean addAll(ByteCollection c) {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }

    public final void clear() {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }

    public final boolean removeAll(ByteCollection c) {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }

    public final boolean removeElement(byte element) {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }

    public final boolean retainAll(ByteCollection c) {
        throw new UnsupportedOperationException("This ByteList is not modifiable.");
    }    
    
    public final ByteList subList(int fromIndex, int toIndex) {
        return UnmodifiableByteList.wrap(getProxiedList().subList(fromIndex,toIndex));
    }

    public final ByteIterator iterator() {
        return UnmodifiableByteIterator.wrap(getProxiedList().iterator());
    }
    
    public ByteListIterator listIterator() {
        return UnmodifiableByteListIterator.wrap(getProxiedList().listIterator());
    }

    public ByteListIterator listIterator(int index) {
        return UnmodifiableByteListIterator.wrap(getProxiedList().listIterator(index));
    }

}
