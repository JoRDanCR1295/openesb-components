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
 * @(#)DtelMapEntryTable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.dtel;

import java.util.*;
import javax.xml.namespace.QName;

public class DtelMapEntryTable {
    private List mEntryList;

    
    /** Creates a new instance of DtelMapEntryTable */
    public DtelMapEntryTable() {
        mEntryList = new ArrayList();
    }
    
    public DtelMapEntry findDtelEntry(QName operation, QName service) {
        // Search by operation's fullname and service's fullname
        for (Iterator i = mEntryList.iterator(); i.hasNext();) {
            DtelMapEntry dtelMapEntry = (DtelMapEntry) i.next();
            QName entryOperation = dtelMapEntry.getOperation();
            QName entryService = dtelMapEntry.getService();
            if (entryOperation.equals(operation) && entryService.equals(service)) {
                return dtelMapEntry;
            } 
        }
        // Cannot find anything that matches operation's fullname and service's fullname
        // Now search by operation's localname and service's fullname
        for (Iterator i = mEntryList.iterator(); i.hasNext();) {
            DtelMapEntry dtelMapEntry = (DtelMapEntry) i.next();
            QName entryOperation = dtelMapEntry.getOperation();
            QName entryService = dtelMapEntry.getService();
            if (entryOperation.getLocalPart().equals(operation.getLocalPart()) && entryService.equals(service)) {
                return dtelMapEntry;
            } 
        }
        return null;
    }
    
    public void addEntry(DtelMapEntry entry) {
       if (!mEntryList.contains(entry)) {
           mEntryList.add(entry);
       }
    }
    
    public void removeEntry(DtelMapEntry entry) {
        mEntryList.remove(entry);
    }
    
    public List getEntryListByServiceUnitName(String serviceUnitName) {
        ArrayList list = new ArrayList();
        for (int i = 0, I = mEntryList.size(); i < I; i++) {
            DtelMapEntry entry = (DtelMapEntry)mEntryList.get(i);
            if (entry.getServiceUnitName().equals(serviceUnitName)) {
                list.add(entry);
            }
        }
        return list;
    }
    
    public List getEntryList() {
        return new ArrayList(mEntryList);
    }
}
