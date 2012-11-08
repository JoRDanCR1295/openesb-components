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
package com.sun.jbi.engine.mashup;

import java.util.*;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import com.sun.jbi.internationalization.Messages;

public class MashupMapEntryTable {

    private List mEntryList;
    private static final Logger logger = Logger.getLogger(MashupMapEntryTable.class.getName());
    private static final Messages mMessages = Messages.getMessages(MashupMapEntryTable.class);

    /** Creates a new instance of DtelMapEntryTable */
    public MashupMapEntryTable() {
        mEntryList = new ArrayList();
    }

    public MashupMapEntry findMashupEntry(QName operation, QName service) {
        logger.fine(mMessages.getString("EDMSE-F0200.findMashupEntry"));
        // Search by operation's fullname and service's fullname
        for (Iterator i = mEntryList.iterator(); i.hasNext();) {
            MashupMapEntry mashupMapEntry = (MashupMapEntry) i.next();
            QName entryOperation = mashupMapEntry.getOperation();
            QName entryService = mashupMapEntry.getService();
            if (entryOperation.equals(operation) && entryService.equals(service)) {
                logger.fine(mMessages.getString("EDMSE-F0201.found_mashupMapEntry"));
                return mashupMapEntry;
            }
        }
        // Cannot find anything that matches operation's fullname and service's fullname
        // Now search by operation's localname and service's fullname
        for (Iterator i = mEntryList.iterator(); i.hasNext();) {
            MashupMapEntry mashupMapEntry = (MashupMapEntry) i.next();
            QName entryOperation = mashupMapEntry.getOperation();
            QName entryService = mashupMapEntry.getService();
            logger.fine(mMessages.getString("EDMSE-F0202.entryOperation") + entryOperation.toString() + mMessages.getString("EDMSE-F0203.entryService") + entryService.toString());
            if (entryOperation.getLocalPart().equals(operation.getLocalPart()) && entryService.equals(service)) {
                logger.fine(mMessages.getString("EDMSE-F0201.found_mashupMapEntry"));
                return mashupMapEntry;
            }
        }
        return null;
    }

    public void addEntry(MashupMapEntry entry) {
        if (!mEntryList.contains(entry)) {
            mEntryList.add(entry);
        }
    }

    public void removeEntry(MashupMapEntry entry) {
        mEntryList.remove(entry);
    }

    public List getEntryListByServiceUnitName(String serviceUnitName) {
        ArrayList list = new ArrayList();
        for (int i = 0, I = mEntryList.size(); i < I; i++) {
            MashupMapEntry entry = (MashupMapEntry) mEntryList.get(i);
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
