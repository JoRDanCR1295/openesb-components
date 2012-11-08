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
 * @(#)TallySet.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

/**
 * Keeps a tally of all items added and removed from the underlying set.
 * 
 * @author Kevan Simpson
 */
public class TallySet {
    private Map<String, TallyEntry> mSet = null;
    
    public TallySet(StringTokenizer tokens) {
        mSet = new HashMap<String, TallyEntry>();
        while (tokens.hasMoreTokens()) {
        	String cleansedStr = tokens.nextToken().trim();
        	if (cleansedStr.length() > 0) {
        		add(cleansedStr);
        	}
        }
    }

    public boolean compareOutput(TallySet actual) {
        List<TallyEntry> unexpected = new ArrayList<TallyEntry>();
        List<TallyEntry> missing = new ArrayList<TallyEntry>();
        
        for (String data : mSet.keySet()) {
            TallyEntry expEntry = mSet.get(data);
            TallyEntry actEntry = actual.mSet.remove(data);
            if (expEntry == null) {
                if (actEntry != null) {
                    unexpected.add(new TallyEntry(actEntry));
                }
            }
            else {
                if (actEntry == null) {
                    missing.add(new TallyEntry(expEntry));
                }
                else {
                    int diff = expEntry.getTally() - actEntry.getTally();
                    if (diff < 0) {
                        unexpected.add(new TallyEntry(data, diff * -1));
                    }
                    else if (diff > 0) {
                        missing.add(new TallyEntry(data, diff));
                    }
                }
            }
        }
        // any extra entries in actual output?
        for (TallyEntry entry : actual.mSet.values()) {
            unexpected.add(entry);
        }
        
        boolean similar = true;
        if (unexpected.size() > 0) {
            similar = false;
            System.out.println("The following rows were NOT EXPECTED in output:");
            for (TallyEntry entry : unexpected) {
                System.out.println(entry.display(false));
            }
        }
        if (missing.size() > 0) {
            similar = false;
            System.out.println("\nThe following rows were MISSING from output:");
            for (TallyEntry entry : missing) {
                System.out.println(entry.display(true));
            }
        }
        if (similar) {
            System.out.println("TEST SUCCESSFUL!  All expected rows occurred in actual output!");
        }
        return similar;
    }
    
    public void add(String data) {
        // ignore persistent point delimiters
        if (data.startsWith("===")) return;
        
        TallyEntry entry = mSet.get(data);
        if (entry == null) {
            mSet.put(data, new TallyEntry(data));
        }
        else {
            entry.increment();
        }
    }
    
    public int getTally(String data) {
        TallyEntry entry = mSet.get(data);
        return (entry == null) ? 0 : entry.getTally();
    }
    
    private static class TallyEntry {
        private String mData = null;
        private int mTally = -1;
        
        public TallyEntry(String data) {
            this(data, 1);
        }
        public TallyEntry(String data, int tally) {
            mTally = tally;
            mData = data;
        }
        public TallyEntry(TallyEntry copy) {
            this(copy.getData(), copy.getTally());
        }
        
        public int getTally() { return mTally; }
        public void increment() { ++mTally; }
        public void decrement() { --mTally; }
        public String getData() { return mData; }
        public String display(boolean missing) {
            StringBuffer buff = new StringBuffer();
            buff.append("\t").append(getData())
                .append(" - ").append(getTally());
            if (missing) buff.append(" missing occurrence(s)");
            else buff.append(" unexpected occurrence(s)");
            
            return buff.toString();
        }
    }
}
