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
 * @(#)BPVisualizerDBHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import com.sun.jbi.cam.plugins.bpelse.DBManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author rdamir
 */
public class BPVisualizerDBHelper {
    
    private DBManager dbManager;
    private final static String BPELPATH_COLUMN = "bpelid";
    private final static String GET_BPEL_PATH_QUERY =
     "select DISTINCT(bpelid) from BPELSE_SCHEMA.STATE  where bpelid like" +
            " '%suName%' "; 

    public BPVisualizerDBHelper() {
        dbManager =  new DBManager();
    }
    
    /*
     * the method return the bpelid from the state table. this value is
     * the actual path to the bpel file in the file system. the will return
     * single value represent the bpel file for the given su
     * @parameter suName - the name of the service unit
     * @return String represent the full path to the bpel file.
     *
     */
    public List<String> getBPELPath(String suName) throws Exception{
       List<String> bpelPathArray = new ArrayList<String>();
       String query = GET_BPEL_PATH_QUERY.replace("suName", suName);
       ResultSet resultset = dbManager.execGenericQuery(query);
       
       if(resultset == null) {
           return bpelPathArray;
       }
        try {
            
            while (resultset.next() ){
                 
                String suidpath = resultset.getString(BPELPATH_COLUMN);
                bpelPathArray.add(suidpath);
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
            throw ex;
       }finally {
           dbManager.closeGenericConnnection();
       }
       return bpelPathArray;
        
    }
}
