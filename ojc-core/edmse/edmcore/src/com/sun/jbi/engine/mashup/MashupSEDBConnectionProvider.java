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
 * @(#)MashupSEDBConnectionProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.mashup;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;
import java.util.logging.Logger;

import com.sun.sql.framework.exception.BaseException;
import com.sun.sql.framework.jdbc.DBConnectionFactory;
import com.sun.sql.framework.jdbc.DBConnectionParameters;
import com.sun.mashup.engine.spi.DBConnectionProvider;


/**
 * @author Sujit Biswas
 *
 */
public class MashupSEDBConnectionProvider implements DBConnectionProvider {

	private static Logger logger = Logger.getLogger(MashupSEDBConnectionProvider.class.getName());

	public void closeConnection(Connection con) {
		if (con != null) {
			try {
				con.close();
			} catch (SQLException e) {
				logger.info(e.getMessage());
			}
		}

	}

	public Connection getConnection(DBConnectionParameters conDef) throws BaseException {
		String driver = conDef.getDriverClass();
		try {
			Class.forName(driver).newInstance();
		} catch (Exception e) {
			logger.info(e.getMessage());
		}

		String username = conDef.getUserName();
		String password = conDef.getPassword();
		String url = conDef.getConnectionURL();
		try {
			return DriverManager.getConnection(url, username, password);
		} catch (SQLException e) {
			logger.info(e.getMessage());
			throw new BaseException(e);
		}

	}

	public Connection getConnection(Properties connProps) throws BaseException {
		String driver = connProps.getProperty(DBConnectionFactory.PROP_DRIVERCLASS);
		try {
			Class.forName(driver).newInstance();
		} catch (Exception e) {
			logger.info(e.getMessage());
		}
		String username = connProps.getProperty(DBConnectionFactory.PROP_USERNAME);
		String password = connProps.getProperty(DBConnectionFactory.PROP_PASSWORD);
		String url = connProps.getProperty(DBConnectionFactory.PROP_URL);
		try {
			return DriverManager.getConnection(url, username, password);
		} catch (SQLException e) {
			logger.info(e.getMessage());
			throw new BaseException(e);
		}
	
	}

}
