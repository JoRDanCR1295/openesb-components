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

package org.axiondb.engine;

import org.axiondb.AxionException;
import org.axiondb.Column;
import org.axiondb.Index;
import org.axiondb.IndexFactory;
import org.axiondb.engine.indexes.BaseIndexFactory;
import org.axiondb.engine.indexes.IntArrayIndex;
import org.axiondb.engine.indexes.ObjectArrayIndex;
import org.axiondb.types.IntegerType;

/**
 * An {@link IndexFactory} for {@link BaseArrayIndex array indices}.
 * 
 * @version  
 * @author Rodney Waldhoff
 * @author Ahimanikya Satapathy
 */
public class ArrayIndexFactory extends BaseIndexFactory implements IndexFactory {
    public ArrayIndexFactory() {
    }

    public Index makeNewInstance(String name, Column col, boolean unique, boolean memorydb) throws AxionException {
        if(col.getDataType() instanceof IntegerType) {
            return new IntArrayIndex(name,col,unique);
        }
        
        return new ObjectArrayIndex(name,col,unique);
    }
}

