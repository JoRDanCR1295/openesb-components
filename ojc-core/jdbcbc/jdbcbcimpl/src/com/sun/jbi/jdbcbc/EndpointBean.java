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
 * @(#)EndpointBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import com.sun.jbi.eManager.provider.EndpointStatus;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import com.sun.jbi.jdbcbc.model.runtime.DBConnectionInfo;


/**
* author
*EndPointBean
**/
public class EndpointBean implements Serializable {
    static final long serialVersionUID = 42L;
    public static final String SERVICE_NAME = "servicename";
    public static final String FULL_SERVICE_NAME = "fullservicename";
    public static final String ENDPOINT_NAME = "endpointname";
    public static final String ENDPOINT_TYPE = "endpointtype";
    public static final String OPERATION = "operation";
    public static final String ENDPOINT_REFERENCE = "endpointreference";
    public static final String URL_CONTEXT = "urlcontext";
    public static final String URL_PORT = "urlport";
    public static final String MESSAGE_EXCHANGE_PATTERN = "mep";
    public static final String ENDPOINT_URL = "endpointurl";
    public static final String WSDL_FILE = "wsdlfile";
    public static final String JDBC_DATABASE_JNDI_NAME = "jndiName";
    public static final String JDBC_TRANSACTION_REQUIRED = "transactionRequired";
  
    public static final String JDBC_DATABASE_DRIVER_CLASS = "driverClassName";
    public static final String JDBC_DATABASE_URL = "dbURL";
    public static final String JDBC_DATABASE_USER = "userName";
    public static final String JDBC_DATABASE_PASSWORD = "password";
    public static final String JDBC_DATABASE_NAME = "databaseName";
    public static final String STATUS = "status";
    public static final String DESCRIPTOR = "descriptor";

    //  A map from the operation name to the meta data (in msg type, out msg type, soap action, faults, etc)
    public static final String OPERATION_NAME_TO_META_DATA = "operationnametometadata";
    public static final String ENDPOINT_TYPE_INBOUND = "inbound";
    public static final String ENDPOINT_TYPE_OUTBOUND = "outbound";
    public static final String STATUS_SHUTDOWN = "SHUTDOWN";
    public static final String STATUS_STOPPED = "STOPPED";
    public static final String STATUS_RUNNING = "RUNNING";
    private final HashMap<String,Object> mConfigTable;
    private transient String mDeploymentId;
    private transient String mTriggerName = null;
    private String mTableName = null;
    private EndpointStatus mEndpointStatus;
    private List mrecordList = new ArrayList();
	private DBConnectionInfo mDBConnectionInfo;

	// throttling related info
	int maxConcurrencyLimit;

    public EndpointBean() {
        //mConfigTable = null;
        mConfigTable = new HashMap<String,Object>();
    }

    protected void setDeploymentId(final String asId) {
        mDeploymentId = asId;
    }

    public String getDeploymentId() {
        return mDeploymentId;
    }

    public void setEndpointStatus(final EndpointStatus val) {
        mEndpointStatus = val;
    }

    public EndpointStatus getEndpointStatus() {
        return mEndpointStatus;
    }

    /**
     * Get the unique name of this endpoint instance
     * @return 
     */
    public String getUniqueName() {
        final String serviceName = getValue(EndpointBean.SERVICE_NAME);
        final String endpointName = getValue(EndpointBean.ENDPOINT_NAME);
        final String endpointType = getValue(EndpointBean.ENDPOINT_TYPE);

        return EndpointBean.getUniqueName(serviceName, endpointName, endpointType);
    }

    /**
     * Utility method to create the unique names with explicit arguments
     * @param aServiceName 
     * @param aEndpointName 
     * @param aEndpointType 
     * @return 
     */
    public static String getUniqueName(final String aServiceName,
        final String aEndpointName, final String aEndpointType) {
        if ((aServiceName != null) && (aEndpointName != null) &&
                (aEndpointType != null)) {
            return aServiceName + "," + aEndpointName + "," + aEndpointType;
        } else {
            return null;
        }
    }

    public void setValue(final String key, String value) {
        if (key == null) {
            return;
        }

        if (value == null) {
            value = "";
        }

        mConfigTable.put(key, value);
    }

    public void setValueObj(final String key, final Object value) {
        if (key == null) {
            return;
        }

        mConfigTable.put(key, value);
    }

    public String getValue(final String key) {
        if (key == null) {
            return null;
        } else {
            return (String) mConfigTable.get(key);
        }
    }

    public Object getValueObj(final String key) {
        return mConfigTable.get(key);
    }

    public void setProcessList(final List recordList) {
        mrecordList = recordList;
    }

    public List getProcessList() {
        return mrecordList;
    }

    public void setTriggerName(final String triggerName) {
        mTriggerName = triggerName;
    }

    public void setTableName(final String tableName) {
        mTableName = tableName;
    }

    public String getTriggerName() {
        return mTriggerName;
    }

    public String getTableName() {
        return mTableName;
    }
    
    public void setDBInfo(DBConnectionInfo dbInfo){
		this.mDBConnectionInfo = dbInfo;
    }

    public DBConnectionInfo getDBInfo(){
		return this.mDBConnectionInfo;
    }

	public void setMaxConcurrencyLimit(int maxConcurrencyLimit){
		this.maxConcurrencyLimit = maxConcurrencyLimit;
	}

	public int getMaxConcurrencyLimit(){
		return this.maxConcurrencyLimit;
	}

}
