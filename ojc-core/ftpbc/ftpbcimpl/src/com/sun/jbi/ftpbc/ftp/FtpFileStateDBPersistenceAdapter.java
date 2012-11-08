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
 * @(#)FtpFileStateDBPersistenceAdapter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/******************************************************************************
 * Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has
 * intellectual property rights relating to technology embodied in the product
 * that is described in this document. In particular, and without limitation,
 * these intellectual property rights may include one or more of the U.S. patents
 * listed at http://www.sun.com/patents and one or more additional patents or
 * pending patent applications in the U.S. and in other countries. THIS PRODUCT
 * CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC.
 * USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN
 * PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial
 * software.  Government users are subject to the Sun Microsystems, Inc. standard
 * license agreement and applicable provisions of the FAR and its supplements.
 * Use is subject to license terms.  This distribution may include materials
 * developed by third parties. Sun, Sun Microsystems, the Sun logo, Java
 * Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 * eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 * Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are
 * used under license and are trademarks or registered trademarks of SPARC
 * International, Inc. in the U.S. and other countries. Products bearing SPARC
 * trademarks are based upon architecture developed by Sun Microsystems, Inc.
 * UNIX is a registered trademark in the U.S. and other countries, exclusively
 * licensed through X/Open Company, Ltd. This product is covered and controlled by
 * U.S. Export Control laws and may be subject to the export or import laws in
 * other countries.  Nuclear, missile, chemical biological weapons or nuclear
 * maritime end uses or end users, whether direct or indirect, are strictly
 * prohibited.  Export or reexport to countries subject to U.S. embargo or to
 * entities identified on U.S. export exclusion lists, including, but not limited
 * to, the denied persons and specially designated nationals lists is strictly
 * prohibited.
 **/
package com.sun.jbi.ftpbc.ftp;

import com.sun.jbi.ftpbc.ftp.statemanager.StatePersistenceAdapter;
import com.sun.jbi.ftpbc.ftp.statemanager.StatePersistenceException;
import java.io.Serializable;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import javax.naming.InitialContext;
//runtime need it: import org.apache.derby.jdbc.ClientDriver;
//import org.apache.derby.jdbc.ClientDriver;
//import org.apache.derby.jdbc.ClientDataSource;

/*
 * A new StatePersistenceAdapter fitting into the existing
 * StateManager/StatePersistenceAdapter framework.
 * It uses relational database as persistence storage.
 * JDBC is used to access the database (derby by default).
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class FtpFileStateDBPersistenceAdapter implements StatePersistenceAdapter {
    private static final Messages mMessages =
            Messages.getMessages(FtpFileStateDBPersistenceAdapter.class);
    private static final Logger mLogger =
            Messages.getLogger(FtpFileStateDBPersistenceAdapter.class);
    private String logMsg;
    protected String componentID = null;
    protected long[] seqNoCache = null;
    protected Connection conn = null;
    
    public FtpFileStateDBPersistenceAdapter(String componentID) throws StatePersistenceException {
        if (componentID == null) {
            throw new StatePersistenceException(mMessages.getString("FTPBC-E006038.ERR_EXT_FTP_NO_COMP_ID", new Object[] {componentID}));
        }
        
        this.conn = null;
        this.componentID = componentID;
        this.seqNoCache = new long[10];
        Arrays.fill(this.seqNoCache, -99999);
        
        this.createTableIfNeeded();

    }
    
    public void save(Serializable state) throws StatePersistenceException {
        FtpFileState ftpState = null;
        try {
            ftpState = (FtpFileState) state;
        } catch (Exception ex) {
            logMsg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"save(Serializable state)", ex});
            mLogger.log(Level.SEVERE, logMsg, ex);
            throw new StatePersistenceException(logMsg, ex);
        }
        
        String where = null;
        String sql = null;
        ResultSet rs = null;
        Statement stmt = null;
        try {
            stmt = this.getConnection().createStatement();
            
            for (int i = 0; i < ftpState.getSequenceNo().length; i++) {
                if (this.seqNoCache[i] == ftpState.getSequenceNo(i)) {
                    // not changed
                    continue;
                }
                where = " WHERE COMPONENT_ID='" + this.componentID + "' AND SEQ_NAME='%" + i + "'";
                sql = "SELECT SEQ_NUMBER FROM \"APP\".\"FTPBC\"" + where;
                rs = stmt.executeQuery(sql);
                if (null == rs || !rs.next()) {
                    sql = "insert into \"APP\".\"FTPBC\" VALUES ('" + this.componentID + "', '%" + i + "', " + ftpState.getSequenceNo(i) + ")";
                } else {
                    sql = "update \"APP\".\"FTPBC\" SET SEQ_NUMBER=" + ftpState.getSequenceNo(i) + where;
                }
                stmt.executeUpdate(sql);
                
            }
        } catch (Exception ex) {
            logMsg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"save(Serializable state)", ex});
            mLogger.log(Level.SEVERE, logMsg, ex);
            throw new StatePersistenceException(logMsg, ex);
        } finally {
            try {
                stmt.close();
            } catch (Exception ex) {
                ;
            }
        }
    }
    
    public Serializable restore() throws StatePersistenceException {
        FtpFileState ftpState = new FtpFileState();
        long seqNo;

        String where = null;
        String sql = null;
        ResultSet rs = null;
        Statement stmt = null;
        try {
            stmt = this.getConnection().createStatement();
            
            for (int i = 0; i < ftpState.getSequenceNo().length; i++) {
                where = " WHERE COMPONENT_ID='" + this.componentID + "' AND SEQ_NAME='%" + i + "'";
                sql = "SELECT SEQ_NUMBER FROM \"APP\".\"FTPBC\"" + where;
                rs = stmt.executeQuery(sql);
                
                if (null == rs || !rs.next()) {
                    continue;
                    //ftpState = null;
                    //break;
                }
                seqNo = rs.getLong("SEQ_NUMBER");
                
                ftpState.setSequenceNo(i, seqNo);
                // set cache
                this.seqNoCache[i] = seqNo;
            }
        } catch (Exception ex) {
            logMsg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"restore()", ex});
            mLogger.log(Level.SEVERE, logMsg, ex);
            throw new StatePersistenceException(logMsg, ex);
        } finally {
            
            try {
                stmt.close();
            } catch (Exception ex) {
                ;
            }
        }

        return ftpState;
        
    }
    
    public void close() throws StatePersistenceException {
        try {
            if (null != this.conn) {
                this.conn.close();
            }
        } catch(Exception ex) {
            logMsg = mMessages.getString("FTPBC-E006001.ERR_EXT_FTP_METHOD_EXCEPTION", new Object[] {"close()", ex});
            mLogger.log(Level.SEVERE, logMsg, ex);
            throw new StatePersistenceException(logMsg, ex);
        } finally {
            this.conn = null;
        }
    }
    
    protected Connection getConnection() throws StatePersistenceException {
        if (null != this.conn) {
            return this.conn;
        }
    
        boolean useDataSource = false;
        try {
            if (useDataSource) {
            	/**
            	String jndi = "to-be-specified"; // required
                InitialContext ic = new InitialContext();
                // or get context from javax.jbi.component.ComponentContext <== 
                //javax.naming.Context ic = javax.jbi.component.ComponentContext.getNamingContext();
                ClientDataSource ds = (ClientDataSource) ic.lookup(jndi); // jdbc/__default
                ds.setPortNumber(1527);
                ds.setServerName("localhost");
                ds.setDatabaseName("sample");
                ds.setUser("app");
                ds.setPassword("app");
                this.conn = ds.getConnection();
                **/
            } else {
                Class.forName("org.apache.derby.jdbc.ClientDriver");
                this.conn = DriverManager.getConnection("jdbc:derby://localhost:1527/sample", "app", "app");
            }
        } catch (Exception ex) {
            logMsg = mMessages.getString("FTPBC-E006039.ERR_EXT_FTP_GET_DB_CONN_EXCEPTION", new Object[] {"getConnection()", ex});
            mLogger.log(Level.SEVERE, logMsg, ex);
            throw new StatePersistenceException(logMsg, ex);
        }

        return this.conn;
        
    }
    
    protected void createTableIfNeeded() throws StatePersistenceException {
        
        String sql = null;
        Statement stmt = null;
        ResultSet rs = null;
        
        try {
            // Check if it already exists.
            sql = "select * from sys.systables where TABLENAME='FTPBC'";
            stmt = this.getConnection().createStatement();
            rs = stmt.executeQuery(sql);
            
            // create table.
            if (null == rs || !rs.next()) {
                sql = "create table \"APP\".\"FTPBC\" (COMPONENT_ID VARCHAR(256), SEQ_NAME VARCHAR(256), SEQ_NUMBER INTEGER, PRIMARY KEY (COMPONENT_ID, SEQ_NAME))";
                stmt.executeUpdate(sql);
            }

        } catch(Exception ex) {
            logMsg = mMessages.getString("FTPBC-E006040.ERR_EXT_FTP_CREATE_TABLE_EXCEPTION", new Object[] {"createTableIfNeeded()", ex});
            mLogger.log(Level.SEVERE, logMsg, ex);
            throw new StatePersistenceException(logMsg, ex);
        } finally {
            try {
                stmt.close();
            } catch (Exception ex) {
                ;
            }
        }
        
        return;
        
    }
    
}
