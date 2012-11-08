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
 * @(#)Schema.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import java.util.ArrayList;
import java.util.List;

/**
 * Schema.java 1.0
 *
 * Interface that extends Component
 *
 * @author Bing Lu
 *
 * @since July 8, 2004
 */
public class Schema {
    private String mName;
    private List<ColumnMetadata> mColumnMetadataList;
    
    public Schema(String name) {
        mName = name;
        mColumnMetadataList = new ArrayList<ColumnMetadata>();
    }

    /**
     * @param columns a list of Strings in the order: (name, type, size, scale)*
     * Use "" for absent values
     */
    public Schema(String name, List<String> columns) {
        this(name);
        setColumnMetadataAsList(columns);
    }

    public String getName() {
        return mName;
    }
    
    public int getColumnCount() {
        return mColumnMetadataList.size();
    }
    
    public ColumnMetadata getColumnMetadata(int i) {
        return mColumnMetadataList.get(i);
    }
    
    public ColumnMetadata getColumnMetadata(String columnName) {
        for (ColumnMetadata cm : mColumnMetadataList) {
            if (cm.getColumnName().equals(columnName)) {
                return cm;
            }
        }
        return null;
    }

    /**
     * @param columns a list of Strings in the order: (name, type, size, scale)*
     */
    public void setColumnMetadataAsList(List<String> columns) {
        mColumnMetadataList.clear();
        for (int i = 0, I = columns.size(); i < I; i+=4) {
            String name = columns.get(i);
            String type = columns.get(i + 1);
            String temp = columns.get(i + 2);
            int size = -1;
            if (temp != null && !temp.equals("")) {
                try {
                    size = Integer.parseInt(temp);
                } catch (Exception e) {
                    size = -1;
                }
            }
            temp = columns.get(i + 3);
            int scale = -1;
            if (temp != null && !temp.equals("")) {
                try {
                    scale = Integer.parseInt(temp);
                } catch (Exception e) {
                    scale = -1;
                }
            }
            mColumnMetadataList.add(new ColumnMetadata(name, type, size, scale));
        }
    }
    
    /**
     * @param columns a list of Strings in the order: (name, type, size, scale)*
     */
    public List<String> getColumnMetadataAsList() throws Exception {
        List<String> list = new ArrayList<String>();
        for (int i = 0, I = mColumnMetadataList.size(); i < I; i++) {
            ColumnMetadata c = mColumnMetadataList.get(i);
            list.add(c.getColumnName());
            list.add(c.getColumnType());
            list.add(c.getColumnSize() + "");
            list.add(c.getColumnScale() + "");
        }
        return list;
    }
        
    public String[] getColumnNames() throws Exception {
        int colCnt = mColumnMetadataList.size();
        String[] ret = new String[colCnt];
        for (int i = 0; i < colCnt; i++) {
            ColumnMetadata c = mColumnMetadataList.get(i);
            ret[i] = c.getColumnName();
        }
        return ret;
    }        
    
    public String[] getColumnTypes() throws Exception {
        int colCnt = mColumnMetadataList.size();
        String[] ret = new String[colCnt];
        for (int i = 0; i < colCnt; i++) {
            ColumnMetadata c = mColumnMetadataList.get(i);
            ret[i] = c.getColumnType();
        }
        return ret;
    }        
    
}    
