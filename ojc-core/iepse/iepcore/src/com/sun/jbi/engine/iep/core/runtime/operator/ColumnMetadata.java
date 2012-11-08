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
 * @(#)ColumnMetadata.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator;

/**
 * ColumnMetadata.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class ColumnMetadata {
    public static int SIZE_NOT_SPECIFIED = -1;
    
    public static int SCALE_NOT_SPECIFIED = -1;

    private String mName;
    private String mType;
    private int mSize;
    private int mScale;
    
    public ColumnMetadata(String name, String type) {
        this(name, type, SIZE_NOT_SPECIFIED, SCALE_NOT_SPECIFIED);
    }
    
    public ColumnMetadata(String name, String type, int size) {
        this(name, type, size, SCALE_NOT_SPECIFIED);
    }

    public ColumnMetadata(String name, String type, int size, int scale) {
        mName = name;
        mType = type;
        mSize = size;
        mScale = scale;
    }

    public String getColumnName() {
        return mName;
    }
    
    public String getColumnType() {
        return mType;
    }
    
    public boolean hasColumnSize() throws Exception {
        if (getColumnSize() < 0) {
            return false;
        }
        return true;
    }
    
    public int getColumnSize() throws Exception {
        return mSize;
    }
    
    public boolean hasColumnScale() throws Exception {
        if (getColumnScale() < 0) {
            return false;
        }
        return true;
    }

    
    public int getColumnScale() throws Exception {
        return mScale;
    }
    
}    
