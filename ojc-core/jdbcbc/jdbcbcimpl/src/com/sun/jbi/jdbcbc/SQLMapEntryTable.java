/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)SQLMapEntryTable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import java.util.*;

import javax.xml.namespace.QName;


public class SQLMapEntryTable {
    private List<SQLMapEntry> mEntryList;

    public SQLMapEntryTable() {
        mEntryList = new ArrayList<SQLMapEntry>();
    }

    private SQLMapEntry findSQLEntry(final QName operation, final QName service) {
        // Search by operation's fullname and service's fullname
        for (final Iterator i = mEntryList.iterator(); i.hasNext();) {
            final SQLMapEntry sqlMapEntry = (SQLMapEntry) i.next();
            final QName entryOperation = sqlMapEntry.getOperation();
            final QName entryService = sqlMapEntry.getService();

            if (entryOperation.equals(operation) &&
                    entryService.equals(service)) {
                return sqlMapEntry;
            }
        }

        // Cannot find anything that matches operation's fullname and service's fullname
        // Now search by operation's localname and service's fullname
        for (final Iterator i = mEntryList.iterator(); i.hasNext();) {
            final SQLMapEntry sqlMapEntry = (SQLMapEntry) i.next();
            final QName entryOperation = sqlMapEntry.getOperation();
            final QName entryService = sqlMapEntry.getService();

            if (entryOperation.getLocalPart().equals(operation.getLocalPart()) &&
                    entryService.equals(service)) {
                return sqlMapEntry;
            }
        }

        return null;
    }

    protected void addEntry(final SQLMapEntry entry) {
        if (!mEntryList.contains(entry)) {
            mEntryList.add(entry);
        }
    }

    protected void removeEntry(final SQLMapEntry entry) {
        mEntryList.remove(entry);
    }

    List<SQLMapEntry> getEntryListByServiceUnitName(final String serviceUnitName) {
        final ArrayList<SQLMapEntry> list = new ArrayList<SQLMapEntry>();

        for (int i = 0, I = mEntryList.size(); i < I; i++) {
            final SQLMapEntry entry = mEntryList.get(i);

            if (entry.getServiceUnitName().equals(serviceUnitName)) {
                list.add(entry);
            }
        }

        return list;
    }

    List<SQLMapEntry> getEntryList() {
        return new ArrayList<SQLMapEntry>(mEntryList);
    }
}
