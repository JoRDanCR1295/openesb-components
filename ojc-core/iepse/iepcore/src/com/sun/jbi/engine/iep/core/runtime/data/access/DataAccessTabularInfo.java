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

package com.sun.jbi.engine.iep.core.runtime.data.access;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author rdwivedi
 */
public class DataAccessTabularInfo {
    private List<String> columnNames = null;
    private List<Object> mDataRows = null;
    public DataAccessTabularInfo(){
        columnNames = new ArrayList<String>();
        mDataRows = new ArrayList<Object>();
    }
    public void setColumns(List<String> list){
        columnNames = list;
    }
    public void addDataRow(List<Object> list){
        mDataRows.add(list);
    }
    
    public List getColumns() {
        return columnNames;
    }
    public List getData() {
        return mDataRows;
    }
    public String getString() {
        StringBuffer buffer  = new StringBuffer() ;
        buffer.append("\n");
        for(int i = 0 ; i < columnNames.size(); i++) {
            buffer.append("\t"+ columnNames.get(i)+"\t");
        }
        buffer.append("\n");
        buffer.append("-------------------------------------------------");
        buffer.append("\n");
        for(int j = 0 ; j < mDataRows.size();j++){
            ArrayList l = (ArrayList) mDataRows.get(j);
            for(int i = 0 ; i < l.size(); i++) {
               buffer.append("\t"+ l.get(i).toString()+"\t") ;
            }
            buffer.append("\n");
        }
        buffer.append("\n");
        buffer.append("-----------------------------------------------------");
        buffer.append("\n");
        return buffer.toString();
    }
    @Override
    public String toString() {
        return getString();
    }
}
