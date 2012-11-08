/*
 * 
 * =======================================================================
 * Copyright (c) 2004-2005 Axion Development Team.  All rights reserved.
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

package org.axiondb.functions;

import org.axiondb.AxionException;
import org.axiondb.DataType;
import org.axiondb.FunctionFactory;
import org.axiondb.Row;
import org.axiondb.RowDecorator;
import org.axiondb.RowIterator;
import org.axiondb.Selectable;
import org.axiondb.types.BooleanType;
import org.axiondb.engine.commands.SubSelectCommand;

/**
 * @version  
 * @author Chuck Burdick
 * @author Amrish Lal
 * @author Rodney Waldhoff
 * @author Ahimanikya Satapathy
 */
public class InFunction extends BaseFunction implements ScalarFunction, FunctionFactory {

    public InFunction() {
        super("IN");
    }

    public ConcreteFunction makeNewInstance() {
        return new InFunction();
    }

    public DataType getDataType() {
        return BOOLEAN_TYPE;
    }

    // NOTE: if multiple columns are used the current impl will not work
    public Object evaluate(RowDecorator row) throws AxionException {
        Object lval = getArgument(0).evaluate(row);
        if (lval == null) {
            // if the value in the row is null, we know we don't match
            return Boolean.FALSE;
        }

        DataType ltype = getArgument(0).getDataType();
        for (int i = 1, I = getArgumentCount(); i < I; i++) {
            Selectable arg = getArgument(i);
            // handle sub-select here
            if (arg instanceof SubSelectCommand) {
                return handleSubSelect((SubSelectCommand) arg, row, lval, ltype);
            } else {
                // else handle list
                try {
                    Object rval = ltype.convert(arg.evaluate(row));
                    if (null != rval && ltype.compare(lval, rval) == 0) {
                        return Boolean.TRUE;
                    }
                }catch(AxionException e) {
                    // ignore, that's OK.
                }
            }
        }
        return Boolean.FALSE;
    }

    private Boolean handleSubSelect(SubSelectCommand cmd, RowDecorator row, Object lval, DataType ltype) throws AxionException {
        RowIterator iter = (RowIterator) cmd.evaluate(row);
        while (iter.hasNext()) {
            Row rrow = iter.next();
            for (int j = 0, J = rrow.size(); j < J; j++) {
                Object rval = rrow.get(j);
                try {
                    rval = ltype.convert(rval);
                    if (null != rval && ltype.compare(lval, rval) == 0) {
                        return Boolean.TRUE;
                    }
                }catch(AxionException e) {
                    // ignore, that's OK.
                }
            }
        }
        return Boolean.FALSE;
    }

    public boolean isValid() {
        return (getArgumentCount() >= 1);
    }

    private static final DataType BOOLEAN_TYPE = new BooleanType();
}
