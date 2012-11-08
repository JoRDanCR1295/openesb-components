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
 * @(#)TestContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.Properties;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import com.sun.jbi.engine.workflow.db.connection.ConnectionProperties;
import com.sun.jbi.engine.workflow.db.dao.DAO;
import com.sun.jbi.engine.workflow.db.dao.DAOFactory;
import com.sun.jbi.engine.workflow.db.test.DBUtil;

public class TestContext extends InitialContext {

    private DataSource mDataSource;
    private HashMap<String, Object> mBindingObjects = new HashMap<String, Object> () ;
    
    public TestContext() throws NamingException {
          // TODO Auto-generated constructor stub
        try {
            mDataSource = getDataSource ();
        } catch (SQLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        mBindingObjects.put("jdbc/__workflow", mDataSource);
    }

    @Override
    public Object lookup(String name) throws NamingException {
        // TODO Auto-generated method stub
        return mBindingObjects.get(name);
    }
    
    private DataSource getDataSource() throws SQLException {
        // TODO Auto-generated method stub
//        Properties props = new Properties ();
//        props.setProperty(ConnectionProperties.DB_TYPE, DAO.DERBY);
//        props.setProperty(ConnectionProperties.DB_DATABASE_NAME, "WORKFLOWDB");
//        props.setProperty(ConnectionProperties.DB_SERVER_NAME, "localhost");
//        props.setProperty(ConnectionProperties.DB_PORT, "1527");
//        props.setProperty(ConnectionProperties.DB_USERNAME, "WORKFLOW");
//        props.setProperty(ConnectionProperties.DB_PASSWORD, "WORKFLOW");       
        
        Properties props = DBUtil.getDBProperties();
        return DAOFactory.getDataSource(props, DAOFactory.getDBType(props.getProperty("DB_Type")));
        
    }    

}
