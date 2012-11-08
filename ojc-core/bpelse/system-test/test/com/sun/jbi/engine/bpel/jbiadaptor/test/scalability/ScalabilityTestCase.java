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
 * @(#)ScalabilityTestCase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.bpel.jbiadaptor.test.scalability;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.BaseTestCase;
import com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Utility;

public abstract class ScalabilityTestCase extends BaseTestCase {
    
    public ScalabilityTestCase(String testName, String enginesDirectory) {
        super(testName, enginesDirectory);
    }

    protected String getScalabilityFlag(List varDbos) {
       return varDbos.size() > 0 ? (String)varDbos.get(0) : null; 
    }
    
    
    protected void verifyVariable(long varId, String actualFlag, String reqdFlag, int actualCount, int reqdCount) {
        String message = "";
        if (actualCount != reqdCount) {
            message = "Check for Var Id : " + varId + " failed, record count does not match. Required : " + reqdCount + " found : " +  actualCount; 
            raiseError(message);
            //assertEquals(reqdCount, actualCount);
        }

        if (reqdCount == 1) {
            if ((actualFlag == null) || !actualFlag.equals(reqdFlag)) {
                message = "Check for Var Id : " + varId + " failed, Scalability Flag in the database does not match expected. Required : " + reqdFlag + " found : " +  actualFlag; 
                raiseError("Persistence database corrrupt");
            }
        }
        message = "Check for Var Id : " + varId + " passed";
        System.out.println(message);
    }
    
    //TODO since this exception is raised on new thread (not main thread)
    // this exception is not getting propagated to the main thread, hence
    // the test is reported to be passed even though the exception is raised here.
    // fix it such that the test fails when this exception happen
    protected boolean raiseError(String message) {
        throw new RuntimeException(message);
    }
    
    protected List getVariableDBOs(long varId) {        
        String query = "Select SCALABILITYPASSIVATED from VARIABLE where VARID = " + varId;
        Connection con = null;
        ResultSet rs = null;
        Statement stmt = null;
        List list = new ArrayList();
        
        String scalabilityFlag = null;
        
        try {
            con = Utility.getConnection();
            stmt = con.createStatement();
            rs = stmt.executeQuery(query);
            
            while (rs.next()) {
                scalabilityFlag = new String(rs.getString(1));
                list.add(scalabilityFlag);
            }
            
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            try {
                if (stmt != null) {
                    stmt.close();
                }
                if (rs != null) {
                    rs.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return list;
    }
}
