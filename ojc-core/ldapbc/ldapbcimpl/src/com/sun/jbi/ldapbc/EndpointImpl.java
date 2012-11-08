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
package com.sun.jbi.ldapbc;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.ldapbc.extensions.LDAPAddress;
import com.sun.jbi.ldapbc.extensions.LDAPBinding;
import java.io.Serializable;
import java.util.HashMap;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;

/**
 * @author
 *EndPointBean
 **/
public class EndpointImpl implements Endpoint, Serializable {

    static final long serialVersionUID = 99L;
    /**
     * 
     */
    public static final String LDAP_CONNECTION = "ldap_connection";
    public static final String SERVICE_NAME = "servicename";
    /**
     * 
     */
    public static final String FULL_SERVICE_NAME = "fullservicename";
    /**
     * 
     */
    public static final String ENDPOINT_NAME = "endpointname";
    /**
     * 
     */
    public static final String ENDPOINT_TYPE = "endpointtype";
    /**
     * 
     */
    public static final String OPERATION = "operation";
    /**
     * 
     */
    public static final String ENDPOINT_REFERENCE = "endpointreference";
    /**
     * 
     */
    public static final String URL_CONTEXT = "urlcontext";
    /**
     * 
     */
    public static final String URL_PORT = "urlport";
    /**
     * 
     */
    public static final String MESSAGE_EXCHANGE_PATTERN = "mep";
    /**
     * 
     */
    public static final String ENDPOINT_URL = "endpointurl";
    /**
     * 
     */
    public static final String WSDL_FILE = "wsdlfile";
    /**
     * 
     */
    public static final String LDAP_SERVER_URL = "ldapUrl";
    /**
     * 
     */
    public static final String STATUS = "status";
    /**
     * 
     */
    public static final String DESCRIPTOR = "descriptor";
    //  A map from the operation name to the meta data (in msg type, out msg type, soap action, faults, etc)
    /**
     * 
     */
    public static final String OPERATION_NAME_TO_META_DATA = "operationnametometadata";
    /**
     * 
     */
    public static final String ENDPOINT_TYPE_INBOUND = "inbound";
    /**
     * 
     */
    public static final String ENDPOINT_TYPE_OUTBOUND = "outbound";
    /**
     * 
     */
    public static final String STATUS_SHUTDOWN = "SHUTDOWN";
    /**
     * 
     */
    public static final String STATUS_STOPPED = "STOPPED";
    /**
     * 
     */
    public static final String STATUS_RUNNING = "RUNNING";
    public static final int SHUTDOWN = 0;
    public static final int STOPPED = 1;
    public static final int RUNNING = 2;
    public static final int INBOUND = 0; // BC is consumer (external proxy consumer)

    public static final int OUTBOUND = 1; // BC is provider (external proxy provider)

    private final HashMap mConfigTable;
    private transient String mDeploymentId;
    private EndpointStatus mEndpointStatus;
    private int mEndpointState;
    private QName mServiceName;
    private int mEndpointType;
    private String mEndpointName;
    private String mServiceUnitId;
    private Definition mDefinition;
    private ServiceEndpoint mServiceEndpoint;
    private transient Document mServiceDescription;
    private LDAPAddress mLDAPAddress;
    private LDAPBinding mLDAPBinding;

    /**
     * 
     */
    public EndpointImpl() {
        //mConfigTable = null;
        mConfigTable = new HashMap();
    }

    /**
     * 
     * @param asId 
     */
    public void setDeploymentId(final String asId) {
        mDeploymentId = asId;
    }

    /**
     * 
     * @return 
     */
    public String getDeploymentId() {
        return mDeploymentId;
    }

    /**
     * 
     * @param val 
     */
    public void setEndpointStatus(final EndpointStatus val) {
        mEndpointStatus = val;
    }

    /**
     * 
     * @return 
     */
    public EndpointStatus getEndpointStatus() {
        return mEndpointStatus;
    }

    public int getState() {
        return mEndpointState;
    }

    public void setState(int state) {
        mEndpointState = state;
    }

    public QName getServiceName() {
        return mServiceName;
    }

    public void setServiceName(QName serviceName) {
        mServiceName = serviceName;
    }

    public String getEndpointName() {
        return mEndpointName;
    }

    public void setEndpointName(String endpointName) {
        mEndpointName = endpointName;
    }

    public int getEndpointType() {
        return mEndpointType;
    }

    public void setEndpointType(int type) {
        mEndpointType = type;
    }

    /**
     * Get the unique name of this endpoint instance
     * @return 
     */
    public String getUniqueName() {
        final String serviceName = getValue(EndpointImpl.SERVICE_NAME);
        final String endpointName = getValue(EndpointImpl.ENDPOINT_NAME);
        final String endpointType = getValue(EndpointImpl.ENDPOINT_TYPE);

        return EndpointImpl.getUniqueName(serviceName, endpointName, endpointType);
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

    /**
     * 
     * @param key 
     * @param value 
     */
    public void setValue(final String key, String value) {
        if (key == null) {
            return;
        }

        if (value == null) {
            value = "";
        }

        mConfigTable.put(key, value);
    }

    /**
     * 
     * @param key 
     * @param value 
     */
    public void setValueObj(final String key, final Object value) {
        if (key == null) {
            return;
        }

        mConfigTable.put(key, value);
    }

    /**
     * 
     * @param key 
     * @return 
     */
    public String getValue(final String key) {
        if (key == null) {
            return null;
        } else {
            return (String) mConfigTable.get(key);
        }
    }

    /**
     * 
     * @param key 
     * @return 
     */
    public Object getValueObj(final String key) {
        return mConfigTable.get(key);
    }

    public static String endpointTypeToString(int endpointType) {
        if (endpointType == EndpointImpl.INBOUND) {
            return EndpointImpl.ENDPOINT_TYPE_INBOUND;
        } else {
            return EndpointImpl.ENDPOINT_TYPE_OUTBOUND;
        }
    }

    public String getServiceUnitID() {
        return mServiceUnitId;
    }

    public void setServiceUnitID(String id) {
        this.mServiceUnitId = id;
    }

    public Definition getDefinition() {
        return mDefinition;
    }

    public void setDefinition(Definition definition) {
        this.mDefinition = definition;
    }

    public ServiceEndpoint getServiceEndpoint() {
        return mServiceEndpoint;
    }

    public void setServiceEndpoint(ServiceEndpoint serviceEndpoint) {
        this.mServiceEndpoint = serviceEndpoint;
    }

    public Document getServiceDescription() {
        return mServiceDescription;
    }

    public void setServiceDescription(Document serviceDescription) {
        this.mServiceDescription = serviceDescription;
    }

    public LDAPAddress getLDAPAddress() {
        return mLDAPAddress;
    }

    public void setLDAPAddress(LDAPAddress address) {
        this.mLDAPAddress = address;
    }

    public LDAPBinding getLDAPBinding() {
        return mLDAPBinding;
    }

    public void setLDAPBinding(LDAPBinding binding) {
        this.mLDAPBinding = binding;
    }
}
