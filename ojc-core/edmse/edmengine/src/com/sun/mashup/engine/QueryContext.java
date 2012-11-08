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
package com.sun.mashup.engine;

import java.util.HashMap;
import java.util.List;

/**
 *
 * @author Anil
 */
public class QueryContext {

    private int pageSize;
    private int pageNumber;
    private int row;
    private String column;
    private String workingDirectory;
    private String dataDirectory;
    private List dynamicParams;
    private List dynamicValues;
    private int offset;
    private HashMap<String, String> dynamicMap;

    public String getColumn() {
        return this.column;
    }

    public void setColumn(String col) {
        this.column = col;
    }

    public List getDynamicParams() {
        return dynamicParams;
    }

    public void setDynamicParams(List params) {
        this.dynamicParams = params;
    }

    public List getDynamicValues() {
        return dynamicValues;
    }

    public void setDynamicValues(List values) {
        this.dynamicValues = values;
    }

    public void setDynamicMap(HashMap<String, String> map) {
        this.dynamicMap = map;
    }

    public HashMap<String, String> getDynamicMap() {
        return this.dynamicMap;
    }

    public String getDataDirectory() {
        return dataDirectory;
    }

    public void setDataDirectory(String dataDir) {
        dataDirectory = dataDir;
    }

    public String getWorkingDirectory() {
        return workingDirectory;
    }

    public void setWorkingDirectory(String workingDir) {
        workingDirectory = workingDir;
    }

    public int getPageSize() {
        return pageSize;
    }

    public void setPageSize(int pageSize) {
        this.pageSize = pageSize;
    }

    public int getPageNumber() {
        return pageNumber;
    }

    public void setPageNumber(int pageNumber) {
        this.pageNumber = pageNumber;
    }

    public int getRow() {
        return row;
    }

    public void setRow(int row) {
        this.row = row;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }
}
