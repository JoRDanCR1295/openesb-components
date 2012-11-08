/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 */
package com.sun.etl.engine.utils;

import java.sql.Connection;
import java.sql.DriverManager;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;
import java.sql.SQLException;

import org.axiondb.AxionException;
import org.axiondb.ExternalConnectionProvider;
import com.sun.jbi.internationalization.Messages;

/**
 * @author srengara
 * @version Revision
 */
public class RuntimeConnectionProvider implements ExternalConnectionProvider {

    private static Logger logger = Logger.getLogger(RuntimeConnectionProvider.class.getName());
    private static final Messages mMessages = Messages.getMessages(RuntimeConnectionProvider.class);
    /**
     * Keep the Constructor public, as Axion will try to instantiate, no-arg constructor.
     */
    public RuntimeConnectionProvider() {
        super();
    }

     /*
     * (non-Javadoc)
     * 
     * @see org.axiondb.ExternalConnectionProvider#getConnection(java.util.Properties)
     */
    public Connection getConnection(Properties spec) throws AxionException  {
        Connection conn = null;
        String jndiResourceName = spec.getProperty(ExternalConnectionProvider.DS_JNDI_NAME);
        
        if(jndiResourceName != null && !"".equalsIgnoreCase(jndiResourceName)) {
            Context ctx;
            try {
                ctx = new InitialContext();
                DataSource ds = (DataSource) ctx.lookup(jndiResourceName);
                conn = ds.getConnection();
            } catch (NamingException ex) {
                Logger.getLogger(RuntimeConnectionProvider.class.getName()).log(Level.SEVERE, null, ex);
            } catch (SQLException e) {
                Logger.getLogger(RuntimeConnectionProvider.class.getName()).log(Level.SEVERE, null, e);
                throw new AxionException(e.getMessage(),e);
            }
        } else {
            try {
                String driverClass = spec.getProperty(ExternalConnectionProvider.PROP_DRIVERCLASS);
                String url = spec.getProperty(ExternalConnectionProvider.PROP_JDBCURL);
                String userName = spec.getProperty(ExternalConnectionProvider.PROP_USERNAME);
                String password = spec.getProperty(ExternalConnectionProvider.PROP_PASSWORD);
                try {
                    Class.forName(driverClass);
                } catch (ClassNotFoundException ex) {
                    Logger.getLogger(RuntimeConnectionProvider.class.getName()).log(Level.SEVERE, null, ex);
                    String nbBundle1 =mMessages.getString("ETLSE-E0445.Unable_to_Load_DriverClass");
                    throw new AxionException(nbBundle1.substring(15), ex);
                }
                conn = DriverManager.getConnection(url, userName, password);
            } catch (SQLException ex) {
                Logger.getLogger(RuntimeConnectionProvider.class.getName()).log(Level.SEVERE, null, ex);
                 String nbBundle2 = mMessages.getString("ETLSE-E0446.Failed_to_create_connection");
                throw new AxionException(nbBundle2.substring(15), ex);
            }
            
        }
        return conn;
    }
}
