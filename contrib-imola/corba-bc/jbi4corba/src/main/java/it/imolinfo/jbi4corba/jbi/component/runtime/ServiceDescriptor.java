 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
/*
 * ServiceDescriptor.java
 *
 */

package it.imolinfo.jbi4corba.jbi.component.runtime;

import javax.jbi.messaging.MessageExchange.Role;
import javax.xml.namespace.QName;

/**
 * This class is a data model of the service that will be used to query the
 * information about the service used in message exchange by service
 * consumer or provider.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class ServiceDescriptor {
    
    /**
     * Creates a new instance of ServiceDescriptor
     */
    public ServiceDescriptor() {
    }
    
    /**
     * Holds value of property EndpointName.
     */
    private String mEndpointName;
    
    /**
     * Getter for property EndpointName.
     *
     * @return Value of property EndpointName.
     */
    public String getEndpointName() {
        return this.mEndpointName;
    }
    
    /**
     * Setter for property EndpointName.
     *
     * @param endpointName New value of property EndpointName.
     */
    public void setEndpointName(String endpointName) {
        this.mEndpointName = endpointName;
    }
    
    /**
     * Holds value of property ServiceName.
     */
    private QName mServiceName;
    
    /**
     * Getter for property ServiceName.
     *
     * @return Value of property ServiceName.
     */
    public QName getServiceName() {
        return this.mServiceName;
    }
    
    /**
     * Setter for property ServiceName.
     *
     * @param serviceName New value of property mServiceName.
     */
    public void setServiceName(QName serviceName) {
        this.mServiceName = serviceName;
    }
    
    /**
     * Holds value of property Role.
     */
    private Role mRole;
    
    /**
     * Getter for property Role.
     *
     * @return Value of property Role.
     */
    public Role getRole() {
        return this.mRole;
    }
    
    /**
     * Setter for property Role.
     *
     * @param role New value of property Role.
     */
    public void setRole(Role role) {
        this.mRole = role;
    }
    
    /**
     * Holds value of property ServiceTypes.
     */
    private QName[] mServiceTypes;
    
    /**
     * Indexed getter for property ServiceTypes.
     *
     * @param index Index of the property.
     * @return Value of the property at <CODE>index</CODE>.
     */
    public QName getServiceTypes(int index) {
        return this.mServiceTypes[index];
    }
    
    /**
     * Getter for property ServiceTypes.
     *
     * @return Value of property ServiceTypes.
     */
    public QName[] getServiceTypes() {
        return this.mServiceTypes;
    }
    
    /**
     * Indexed setter for property mServiceTypes.
     *
     * @param index Index of the property.
     * @param serviceTypes New value of the property at <CODE>index</CODE>.
     */
    public void setServiceTypes(int index, QName serviceTypes) {
        this.mServiceTypes[index] = serviceTypes;
    }
    
    /**
     * Setter for property mServiceTypes.
     *
     * @param serviceTypes New value of property ServiceTypes.
     */
    public void setServiceTypes(QName[] serviceTypes) {
        this.mServiceTypes = serviceTypes;
    }
    /**
     * returns a first servicetype in the servicetype array
     * @return service type Qname or null if no servicetypes are initialized
     */
    public QName getServiceType() {
        QName[] types = this.getServiceTypes();
        QName serviceType = null;
        if ( types != null && types.length > 0 ) {
            serviceType = types[0];
        }
        return serviceType;
    }
    /**
     * Holds value of property Operations.
     */
    private OperationDescriptor[] mOperations;
    
    /**
     * Indexed getter for property Operations.
     *
     * @param index Index of the property.
     * @return Value of the property at <CODE>index</CODE>.
     */
    public OperationDescriptor getOperations(int index) {
        return this.mOperations[index];
    }
    
    /**
     * Getter for property mOperations.
     *
     * @return Value of property mOperations.
     */
    public OperationDescriptor[] getOperations() {
        return this.mOperations;
    }
    
    /**
     * Indexed setter for property mOperations.
     *
     * @param index Index of the property.
     * @param operations New value of the property at <CODE>index</CODE>.
     */
    public void setOperations(int index, OperationDescriptor operations) {
        this.mOperations[index] = operations;
    }
    
    /**
     * Setter for property mOperations.
     *
     * @param operations New value of property mOperations.
     */
    public void setOperations(OperationDescriptor[] operations) {
        this.mOperations = operations;
    }
    
    
    public static final class OperationDescriptor {
        
        /** In Only MEP. */
        public static final String IN_ONLY_MEP =  "http://www.w3.org/2004/08/wsdl/in-only";
        /** Robust In Only MEP.   */
        public static final String ROBUST_IN_ONLY_MEP =  "http://www.w3.org/2004/08/wsdl/robust-in-only";
        
        /** In Out MEP. */
        public static final String IN_OUT_MEP = "http://www.w3.org/2004/08/wsdl/in-out";
        /** In Optional Out MEP.   */
        public static final String IN_OPTIONAL_OUT_MEP = "http://www.w3.org/2004/08/wsdl/in-opt-out";
        
        /**
         * Holds value of property mServiceType.
         */
        private QName mServiceType;
        
        /**
         * Getter for property mName.
         *
         * @return Value of property mName.
         */
        public QName getServiceType() {
            return this.mServiceType;
        }
        
        /**
         * Setter for property ServiceType.
         *
         * @param serviceType New value of property ServiceType.
         */
        public void setServiceType(QName serviceType) {
            this.mServiceType = serviceType;
        }
        
        /**
         * Holds value of property mName.
         */
        private String mName;
        
        /**
         * Getter for property mName.
         *
         * @return Value of property mName.
         */
        public String getName() {
            return this.mName;
        }
        
        /**
         * Setter for property mName.
         *
         * @param mName New value of property mName.
         */
        public void setName(String name) {
            this.mName = name;
        }
        
        /**
         * Holds value of property mInput.
         */
        private String mInput;
        
        /**
         * Getter for property mInput.
         *
         * @return Value of property mInput.
         */
        public String getInput() {
            return this.mInput;
        }
        
        /**
         * Setter for property mInput.
         *
         * @param mInput New value of property mInput.
         */
        public void setInput(String input) {
            this.mInput = input;
        }
        
        /**
         * Holds value of property mOutput.
         */
        private String mOutput;
        
        /**
         * Getter for property mOutput.
         *
         * @return Value of property mOutput.
         */
        public String getOutput() {
            return this.mOutput;
        }
        
        /**
         * Setter for property mOutput.
         *
         * @param mOutput New value of property mOutput.
         */
        public void setOutput(String output) {
            this.mOutput = output;
        }
        
        /**
         * Holds value of property mMEP.
         */
        private String mMEP;
        
        /**
         * Getter for property mMEP.
         *
         * @return Value of property mMEP.
         */
        public String getMEP() {
            return this.mMEP;
        }
        
        /**
         * Setter for property mMEP.
         *
         * @param mMEP New value of property mMEP.
         */
        public void setMEP(String MEP) {
            this.mMEP = MEP;
        }
        
    }
    
}
