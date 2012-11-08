/*
 * 
 * =======================================================================
 * Copyright (c) 2002-2004 Axion Development Team.  All rights reserved.
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

package org.axiondb.engine.commands;

import java.util.List;

import org.axiondb.AxionException;
import org.axiondb.Column;
import org.axiondb.ColumnIdentifier;
import org.axiondb.Constraint;
import org.axiondb.Database;
import org.axiondb.Index;
import org.axiondb.Table;
import org.axiondb.TableIdentifier;
import org.axiondb.constraints.ForeignKeyConstraint;
import org.axiondb.constraints.UniqueConstraint;
import org.axiondb.util.StringIdentifierGenerator;

/**
 * An <code>ADD CONSTRAINT</code> command, as generated by <code>ALTER TABLE</code>
 * and <code>CREATE TABLE</code> statements.
 * 
 * @version  
 * @author James Strachan
 * @author Rodney Waldhoff
 * @author Jonathan Giron
 * @author Ahimanikya Satapathy
 */
public class AddConstraintCommand extends ConstraintCommand {
    public AddConstraintCommand(String tableName, Constraint constraint) {
        super(tableName);
        setConstraint(constraint);
    }

    public Constraint getConstraint() {
        return _constraint;
    }

    private void setConstraint(Constraint c) {
        _constraint = c;
    }

    protected void execute(Database db, Table table) throws AxionException {
        assertNotReadOnly(db);
        if (null == getConstraint()) {
            throw new AxionException("Constraint must not be null.");
        }

        getConstraint().resolve(db, new TableIdentifier(getTableName()));
        table.addConstraint(getConstraint()); // first add the constraint

        // Automagically add btree index before adding the constraint if it is a PK or
        // unique or FK, and an index doesn't already exist for its column. This should
        // speed up evaluation of constraints on tables with existing data, as the
        // matching algorithm takes advantage of indexes.
        if (getConstraint() instanceof UniqueConstraint) {
            UniqueConstraint pk = (UniqueConstraint) getConstraint();
            boolean isUnique = pk.getSelectableCount() == 1;
            buildIndexIfRequired(db, pk.getSelectableList(), table, isUnique);
        } else if (getConstraint() instanceof ForeignKeyConstraint) {
            ForeignKeyConstraint fk = (ForeignKeyConstraint) getConstraint();
            if (!fk.getParentTableName().equals(fk.getChildTableName())) {
                Table parentTable = db.getTable(fk.getParentTableName());
                parentTable.addConstraint(fk);
            }

            List childColumns = fk.getChildTableColumns();
            Table childTable = db.getTable(fk.getChildTableName());
            buildIndexIfRequired(db, childColumns, childTable, false);
        }
    }

    private void buildIndexIfRequired(Database db, List columns, Table table, boolean isUnique) throws AxionException {
        boolean foundIndex = false;
        for (int i = 0, I = columns.size(); i < I; i++) {
            Object sel = columns.get(i);
            if (sel instanceof ColumnIdentifier) {
                ColumnIdentifier columnId = (ColumnIdentifier) sel;
                Column column = table.getColumn(columnId.getName());
                if (table.isColumnIndexed(column)) {
                    foundIndex = true;
                    break;
                }
            }
        }

        if (!foundIndex && columns.size() >= 1) {
            Object sel = columns.get(0);
            if (sel instanceof ColumnIdentifier) {
                ColumnIdentifier columnId = (ColumnIdentifier) sel;
                Column column = table.getColumn(columnId.getName());
                if (!table.isColumnIndexed(column)) {
                    boolean isMemorydb = db.getDBDirectory() == null;
                    String name = StringIdentifierGenerator.INSTANCE.next16DigitIdentifier("SYS");
                    Index index = db.getIndexFactory("default").makeNewInstance(name, column, isUnique, isMemorydb);
                    if (index != null) {
                        db.addIndex(index, table, true);
                    }
                }
            }
        }
    }

    private Constraint _constraint;
}
