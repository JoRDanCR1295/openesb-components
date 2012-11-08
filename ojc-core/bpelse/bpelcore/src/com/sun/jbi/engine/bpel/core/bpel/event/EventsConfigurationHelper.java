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
 * @(#)EventsConfigurationHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.event;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.EventsFilterImpl;
import com.sun.jbi.engine.bpel.core.bpel.management.util.SchemaConstants;
import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
*
* @author Sun Microsystems
*/
public class EventsConfigurationHelper {

	private static final String SELECT_PROCESS_EVENT_FLAG = "SELECT " + SchemaConstants.GEN_BPEL_EVENTS + " FROM "
		+ MonitorDBSchemaCreation.MONITOR_BPEL_PROCESS + " WHERE " + SchemaConstants.BPEL_ID + " = ?";

	private static final String INSERT_MONITOR_PROCESS = "INSERT INTO " + MonitorDBSchemaCreation.MONITOR_BPEL_PROCESS 
		+ " (" + SchemaConstants.SU_NAME + ", " + SchemaConstants.BPEL_ID + ", " +  SchemaConstants.GEN_BPEL_EVENTS 
		+ ") VALUES (?, ?, ?)";

	private static final String UPDATE_PROCESS_EVENT_FLAG = "UPDATE " + MonitorDBSchemaCreation.MONITOR_BPEL_PROCESS
		+ " SET " + SchemaConstants.GEN_BPEL_EVENTS + " = ? WHERE " + SchemaConstants.BPEL_ID + " = ?"; 

	private static final String SELECT_ALL_PROCESS_EVENT_FLAG = "SELECT " + SchemaConstants.BPEL_ID + ", " 
		+ SchemaConstants.SU_NAME + ", " + SchemaConstants.GEN_BPEL_EVENTS + " FROM " 
		+ MonitorDBSchemaCreation.MONITOR_BPEL_PROCESS;
	
	private static final String DELETE_PROCESS_EVENT_FLAG = "DELETE FROM " 
		+ MonitorDBSchemaCreation.MONITOR_BPEL_PROCESS + " WHERE " + SchemaConstants.SU_NAME + " = ?";


	/**
	 * 
	 * @param connFactory
	 * @param configCol
	 * @throws Exception
	 */
	public static void persistEventsGenerationFlag(DBConnectionFactory connFactory, 
			Collection<ProcessEventsConfig> configCol) throws Exception {
		
		PreparedStatement query = null;
		PreparedStatement updateEventFlag = null;
		PreparedStatement insertProcess = null;
		ResultSet resultSet = null;
		Connection conn = null;
		AbstractDBConnection abstractConn = null;

		try {
			abstractConn = connFactory.createNonXAConnection();
			conn = abstractConn.getUnderlyingConnection();
			conn.setAutoCommit(false);
			query = conn.prepareStatement(SELECT_PROCESS_EVENT_FLAG);
			
			for (ProcessEventsConfig config : configCol) {
				query.setString(1, config.getBPELProcessId().toString());
				resultSet = query.executeQuery();
				if (resultSet.next()) {
					// update
					String curFlag = resultSet.getString(1);
					boolean curVal = ("Y".equals(curFlag)) ? true : false;
					if (curVal != config.getProcessEventsFlag()) {
						// update value. UPDATE_PROCESS_EVENT_FLAG
						updateEventFlag = conn.prepareStatement(UPDATE_PROCESS_EVENT_FLAG);
						updateEventFlag.setString(1, (config.getProcessEventsFlag()) ? "Y" : "N");
						updateEventFlag.setString(2, config.getBPELProcessId().toString());
						updateEventFlag.execute();

					} // else do nothing. value is already set to desired val.
				} else {
					// insert
					insertProcess = conn.prepareStatement(INSERT_MONITOR_PROCESS);
					insertProcess.setString(1, config.getServiceUnitName());
					insertProcess.setString(2, config.getBPELProcessId().toString());
					insertProcess.setString(3, (config.getProcessEventsFlag()) ? "Y" : "N");
					insertProcess.execute();
				}
			}

			conn.commit();

		} catch (Exception e) {
			rollback(conn);
			throw new RuntimeException("Error.", e);
		} finally {
			closeResultSet(resultSet);
			closeStatement(query);
			closeStatement(updateEventFlag);
			closeStatement(insertProcess);
			closeConnection(abstractConn);
		}
	}
	
	/**
	 * 
	 * @param connFactory
	 * @param serviceUnit
	 */
	public static void deleteEventsGenerationFlag(DBConnectionFactory connFactory, String serviceUnit) {
		PreparedStatement deleteStmt = null;
		Connection conn = null;
		AbstractDBConnection abstractConn = null;

		try {
			abstractConn = connFactory.createNonXAConnection();
			conn = abstractConn.getUnderlyingConnection();
			conn.setAutoCommit(false);

			deleteStmt = conn.prepareStatement(DELETE_PROCESS_EVENT_FLAG);
			deleteStmt.setString(1, serviceUnit);
			deleteStmt.execute();

			conn.commit();

		} catch (Exception e) {
			rollback(conn);
			throw new RuntimeException("Error.", e);
		} finally {
			closeStatement(deleteStmt);
			closeConnection(abstractConn);
		}
	}
	
	/**
	 * 
	 * @param connFactory
	 * @return
	 */
	public static Map<QName, ProcessEventsConfig> retrieveEventsGenerationFlag(DBConnectionFactory connFactory) {
		PreparedStatement query = null;
		ResultSet resultSet = null;
		Connection conn = null;
		AbstractDBConnection abstractConn = null;
		Map<QName, ProcessEventsConfig> configMap = new HashMap<QName, ProcessEventsConfig>();

		try {
			abstractConn = connFactory.createNonXAConnection();
			conn = abstractConn.getUnderlyingConnection();
			conn.setAutoCommit(false);

			query = conn.prepareStatement(SELECT_ALL_PROCESS_EVENT_FLAG);
			resultSet = query.executeQuery();
			while (resultSet.next()) {
				// update
				String bpelId = resultSet.getString(1);
				String suName = resultSet.getString(2);
				boolean eventFlag = ("Y".equals(resultSet.getString(3))) ? true : false;
				QName processQName; 
				try {
		            processQName = QName.valueOf(bpelId);
		        } catch (Exception e) {
		            String errorMsg = I18n.loc("BPCOR-6112: bpel process name: {0} is not a well formed QName", bpelId);
		            throw new Exception(errorMsg);
		        }
		        ProcessEventsConfig config = new ProcessEventsConfig(processQName, suName);
		        config.setProcessEventsFlag(eventFlag);
		        configMap.put(processQName, config);
			}
		} catch (Exception e) {
			throw new RuntimeException("Error.", e);
		} finally {
			closeResultSet(resultSet);
			closeStatement(query);
			closeConnection(abstractConn);
		}
		
		return (configMap.isEmpty()) ? null : configMap;
		
	}
	
	/**
	 * 
	 * @param engine
	 * @param configCol
	 */
	public static void syncEventsGenerationFlag(Engine engine, Collection<ProcessEventsConfig> configCol) {
		for (ProcessEventsConfig config : configCol) {
			BPELProcessManager processManager = engine.getBPELProcessManager(config.getBPELProcessId());
			if (processManager != null) {
				boolean eventFlag = config.getProcessEventsFlag();
				processManager.setGenerateEventsFlag(eventFlag);
			} else {
				// Log Error but no need to throw exception.
			}
		}
	}
	
	/**
	 * 
	 * @param bProc
	 * @return
	 */
	public static EventsFilter createEventsFilter(RBPELProcess bProc) {
		Set<Long> varSet = new HashSet<Long>();
		Set<Long> actSet = new HashSet<Long>();
		processBPELElement(bProc, actSet, varSet);
		if (!(actSet.size() > 0)) {
			actSet = null;
		}
		if (!(varSet.size() > 0)) {
			varSet = null;
		}
		EventsFilter eventFilter = new EventsFilterImpl(actSet, varSet);
		return eventFilter;
	}
	
	/*
	 * 
	 */
	private static void processBPELElement(BPELElement element, Set<Long> actSet, Set<Long> varSet) {
		List childList = element.getChildren();
    	if (childList != null) {
    		for (int i = 0; i < childList.size(); i++) {
    			Object obj = childList.get(i);

    			if (obj instanceof RVariable) {
    				RVariable rvar = (RVariable) obj;
    				long id = rvar.getUniqueId();
    				boolean genEvents = ("yes".equals(rvar.getGenerateEvents())) ? true : false;
    				if (genEvents) {
    					varSet.add(new Long(id));
    				}
    			} else if (obj instanceof RActivity) {
    				RActivity ract = (RActivity) obj;
    				long id = ract.getUniqueId();
    				boolean genEvents = ("yes".equals(ract.getGenerateEvents())) ? true : false;
    				if (genEvents) {
    					actSet.add(new Long(id));
    				}
    				processBPELElement((BPELElement) obj, actSet, varSet);
    			} else if (obj instanceof BPELElement) {
    				processBPELElement((BPELElement) obj, actSet, varSet);
    			}
    		}
    	}
	}         

	/*
	 * 
	 */
	private static void rollback(Connection conn) {
		if (conn != null) {
			try {
				conn.rollback();
			} catch (SQLException se) {
				se.printStackTrace();
			}
		}
	}
	
	/*
	 * 
	 */
	private static void closeStatement(Statement stm) {
		if (stm != null) {
			try {
				stm.close();
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
	}

	/*
	 * 
	 */
	private static void closeResultSet(ResultSet resultSet) {
		if (resultSet != null) {
			try {
				resultSet.close();
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
	}

	/*
	 * 
	 */
	private static void closeConnection(AbstractDBConnection conn) {
		if (conn != null) {
			try {
				conn.close();
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
	}
}
