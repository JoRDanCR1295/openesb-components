 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.utils;

import javax.xml.namespace.QName;

/**
 * Holder for Enpoint Reference information
 * This Class Create an Holder that mantain all the information about to EPR
 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
 */
public class EPREndpointInfo {
    
    /**
     * The IOR
     */
    private String _ior;
    
    /**
     * The Endpoint InterfaceName
     */
    private QName _interfacename;
    
    /**
     * The Endpointname
     */
    private String _endpointName;
    
    
    /**
     * Default Constructor
     */
    public EPREndpointInfo(){
    
    }
    
    /**
     * 
     **/
    public EPREndpointInfo(String address,QName intname,String endpoitname ){
        _ior=address;
        _interfacename=intname;
        _endpointName=endpoitname;
    }
    
    /**
     * Return the IOR 
     * @return  String IOR
     **/
    public String getIor() {
        return _ior;
    }

    /**
     * @param String IOR
     **/
    public void setIor(String ior) {
        this._ior = ior;
    }

    /**
     * Return the interface name
     * @return the return
     **/
    public QName getInterfacename() {
        return _interfacename;
    }

    /**
     * Set interface Name
     * @param interfacename 
     **/
    public void setInterfacename(QName interfacename) {
        this._interfacename = interfacename;
    }

    /**
     * get the EndpointName
     * 
     **/
    public String getEndpointName() {
        return _endpointName;
    }

    /**
     * Set EndpointName
     * @param Stirng 
     **/
    public void setEndpointName(String endpointName) {
        this._endpointName = endpointName;
    }

}
