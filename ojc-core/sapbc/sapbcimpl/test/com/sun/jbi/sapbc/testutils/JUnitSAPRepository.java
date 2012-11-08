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
 * @(#)JUnitSAPRepository.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.testutils;

import com.sap.mw.jco.IFunctionTemplate;
import com.sap.mw.jco.IRepository;
import com.sap.mw.jco.JCO;

/**
 * Utility class for implementing our own SAP repository to be used with the jUnit
 * tests.
 *
 * @author Julie Knight (julie.knight@sun.com)
 */
public class JUnitSAPRepository extends JCO.BasicRepository implements IRepository {
    
    /**
     * Creates a new empty repository
     */
    public JUnitSAPRepository(String name) {
        super(name);
        //System.out.println("Initializing repository for " + name);
        initRepository();
    }
    
     // Adds a RFM interface to the local Repository cache. 
     public synchronized void addFunctionInterfaceToCache(com.sap.mw.jco.IMetaData meta) {
       super.addFunctionInterfaceToCache(meta);
     } 
     
    /** The repository we gonna be using */
    //protected static IRepository repository;
    
    private void initRepository() {
    //static {
        
        //repository = new JUnitSAPRepository("JUnitSAPRepository");
        JCO.MetaData fmeta = null;
        JCO.MetaData smeta =  null;
        
        // Unicode definition of functions. The server with this repository can
        // dispatch calls only from unicode systems.
        // The format of this code are complements of SAP BAPI 5.0.3
        
        //------------------------------------------------------------------------------
        //  Add function 'BAPI_FLIGHT_GETDETAIL'
        //------------------------------------------------------------------------------
        fmeta = new JCO.MetaData("BAPI_FLIGHT_GETDETAIL");
        fmeta.addInfo("AIRLINEID", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 6, 0, 0, null, "AIRLINEID", com.sap.mw.jco.JCO.IMPORT_PARAMETER, null, null);
        fmeta.addInfo("CONNECTIONID", com.sap.mw.jco.JCO.TYPE_NUM, 4, 8, 0, 0, null, "CONNECTIONID", com.sap.mw.jco.JCO.IMPORT_PARAMETER, null, null);
        fmeta.addInfo("FLIGHTDATE", com.sap.mw.jco.JCO.TYPE_DATE, 8, 16, 0, 0, null, "FLIGHTDATE", com.sap.mw.jco.JCO.IMPORT_PARAMETER, null, null);
        fmeta.addInfo("FLIGHT_DATA", com.sap.mw.jco.JCO.TYPE_STRUCTURE, 115, 230, 0, 0, null, "FLIGHT_DATA", com.sap.mw.jco.JCO.EXPORT_PARAMETER, "BAPISFLDAT", null);
        fmeta.addInfo("ADDITIONAL_INFO", com.sap.mw.jco.JCO.TYPE_STRUCTURE, 22, 44, 0, 0, null, "ADDITIONAL_INFO", com.sap.mw.jco.JCO.EXPORT_PARAMETER, "BAPISFLADD", null);
        fmeta.addInfo("AVAILIBILITY", com.sap.mw.jco.JCO.TYPE_STRUCTURE, 12, 24, 0, 0, null, "AVAILIBILITY", com.sap.mw.jco.JCO.EXPORT_PARAMETER, "BAPISFLAVA", null);
        fmeta.addInfo("EXTENSION_IN", com.sap.mw.jco.JCO.TYPE_TABLE, 990, 1980, 0, 0, null, "EXTENSION_IN", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, "BAPIPAREX", null);
        fmeta.addInfo("EXTENSION_OUT", com.sap.mw.jco.JCO.TYPE_TABLE, 990, 1980, 0, 0, null, "EXTENSION_OUT", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, "BAPIPAREX", null);
        fmeta.addInfo("RETURN", com.sap.mw.jco.JCO.TYPE_TABLE, 544, 1088, 0, 0, null, "RETURN", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, "BAPIRET2", null);
        addFunctionInterfaceToCache(fmeta);
        
        //------------------------------------------------------------------------------
        // Add the structure BAPISFLDAT to the structure cache
        //------------------------------------------------------------------------------
        smeta =  new com.sap.mw.jco.JCO.MetaData("BAPISFLDAT");
        smeta.addInfo("AIRLINEID", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 6, 0, 0, null, "AIRLINEID", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("AIRLINE", com.sap.mw.jco.JCO.TYPE_CHAR, 20, 40, 6, 0, null, "AIRLINE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("CONNECTID", com.sap.mw.jco.JCO.TYPE_NUM, 4, 8, 46, 0, null, "CONNECTID", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("FLIGHTDATE", com.sap.mw.jco.JCO.TYPE_DATE, 8, 16, 54, 0, null, "FLIGHTDATE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("AIRPORTFR", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 6, 70, 0, null, "AIRPORTFR", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("CITYFROM", com.sap.mw.jco.JCO.TYPE_CHAR, 20, 40, 76, 0, null, "CITYFROM", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("AIRPORTTO", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 6, 116, 0, null, "AIRPORTTO", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("CITYTO", com.sap.mw.jco.JCO.TYPE_CHAR, 20, 40, 122, 0, null, "CITYTO", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("DEPTIME", com.sap.mw.jco.JCO.TYPE_TIME, 6, 12, 162, 0, null, "DEPTIME", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("ARRTIME", com.sap.mw.jco.JCO.TYPE_TIME, 6, 12, 174, 0, null, "ARRTIME", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("ARRDATE", com.sap.mw.jco.JCO.TYPE_DATE, 8, 16, 186, 0, null, "ARRDATE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("PRICE", com.sap.mw.jco.JCO.TYPE_BCD, 6, 12, 202, 4, null, "PRICE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("CURR", com.sap.mw.jco.JCO.TYPE_CHAR, 5, 10, 214, 0, null, "CURR", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("CURR_ISO", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 6, 224, 0, null, "CURR_ISO", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        this.addStructureDefinitionToCache(smeta);
        
        //------------------------------------------------------------------------------
        // Add the structure BAPISFLADD to the structure cache
        //------------------------------------------------------------------------------
        smeta =  new com.sap.mw.jco.JCO.MetaData("BAPISFLADD");
        smeta.addInfo("FLIGHTTIME", com.sap.mw.jco.JCO.TYPE_INT, 2, 4, 0, 0, null, "FLIGHTTIME", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("DISTANCE", com.sap.mw.jco.JCO.TYPE_BCD, 2, 5, 4, 4, null, "DISTANCE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("UNIT", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 6, 10, 0, null, "UNIT", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("UNIT_ISO", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 6, 16, 0, null, "UNIT_ISO", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("PLANETYPE", com.sap.mw.jco.JCO.TYPE_CHAR, 10, 20, 22, 0, null, "PLANETYPE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("FLIGHTTYPE", com.sap.mw.jco.JCO.TYPE_CHAR, 1, 2, 42, 0, null, "FLIGHTTYPE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        this.addStructureDefinitionToCache(smeta);
        
        //------------------------------------------------------------------------------
        // Add the structure BAPISFLAVA to the structure cache
        //------------------------------------------------------------------------------
        smeta =  new com.sap.mw.jco.JCO.MetaData("BAPISFLAVA");
        smeta.addInfo("ECONOMAX", com.sap.mw.jco.JCO.TYPE_INT, 2, 4, 0, 0, null, "ECONOMAX", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("ECONOFREE", com.sap.mw.jco.JCO.TYPE_INT, 2, 4, 4, 0, null, "ECONOFREE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("BUSINMAX", com.sap.mw.jco.JCO.TYPE_INT, 2, 4, 8, 0, null, "BUSINMAX", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("BUSINFREE", com.sap.mw.jco.JCO.TYPE_INT, 2, 4, 12, 0, null, "BUSINFREE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("FIRSTMAX", com.sap.mw.jco.JCO.TYPE_INT, 2, 4, 16, 0, null, "FIRSTMAX", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        smeta.addInfo("FIRSTFREE", com.sap.mw.jco.JCO.TYPE_INT, 2, 4, 20, 0, null, "FIRSTFREE", com.sap.mw.jco.JCO.EXPORT_PARAMETER, null, null);
        this.addStructureDefinitionToCache(smeta);
        
        //------------------------------------------------------------------------------
        // Add the structure BAPIPAREX to the structure cache
        //------------------------------------------------------------------------------
        smeta =  new com.sap.mw.jco.JCO.MetaData("BAPIPAREX");
        smeta.addInfo("STRUCTURE", com.sap.mw.jco.JCO.TYPE_CHAR, 30, 60, 0, 0, null, "STRUCTURE", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("VALUEPART1", com.sap.mw.jco.JCO.TYPE_CHAR, 240, 480, 60, 0, null, "VALUEPART1", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("VALUEPART2", com.sap.mw.jco.JCO.TYPE_CHAR, 240, 480, 540, 0, null, "VALUEPART2", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("VALUEPART3", com.sap.mw.jco.JCO.TYPE_CHAR, 240, 480, 1020, 0, null, "VALUEPART3", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("VALUEPART4", com.sap.mw.jco.JCO.TYPE_CHAR, 240, 480, 1500, 0, null, "VALUEPART4", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        this.addStructureDefinitionToCache(smeta);
        
        //------------------------------------------------------------------------------
        // Add the structure BAPIRET2 to the structure cache
        //------------------------------------------------------------------------------
        smeta =  new com.sap.mw.jco.JCO.MetaData("BAPIRET2");
        smeta.addInfo("TYPE", com.sap.mw.jco.JCO.TYPE_CHAR, 1, 2, 0, 0, null, "TYPE", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("ID", com.sap.mw.jco.JCO.TYPE_CHAR, 20, 40, 2, 0, null, "ID", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("NUMBER", com.sap.mw.jco.JCO.TYPE_NUM, 3, 6, 42, 0, null, "NUMBER", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MESSAGE", com.sap.mw.jco.JCO.TYPE_CHAR, 220, 440, 48, 0, null, "MESSAGE", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("LOG_NO", com.sap.mw.jco.JCO.TYPE_CHAR, 20, 40, 488, 0, null, "LOG_NO", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("LOG_MSG_NO", com.sap.mw.jco.JCO.TYPE_NUM, 6, 12, 528, 0, null, "LOG_MSG_NO", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MESSAGE_V1", com.sap.mw.jco.JCO.TYPE_CHAR, 50, 100, 540, 0, null, "MESSAGE_V1", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MESSAGE_V2", com.sap.mw.jco.JCO.TYPE_CHAR, 50, 100, 640, 0, null, "MESSAGE_V2", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MESSAGE_V3", com.sap.mw.jco.JCO.TYPE_CHAR, 50, 100, 740, 0, null, "MESSAGE_V3", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MESSAGE_V4", com.sap.mw.jco.JCO.TYPE_CHAR, 50, 100, 840, 0, null, "MESSAGE_V4", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("PARAMETER", com.sap.mw.jco.JCO.TYPE_CHAR, 32, 64, 940, 0, null, "PARAMETER", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("ROW", com.sap.mw.jco.JCO.TYPE_INT, 2, 4, 1004, 0, null, "ROW", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("FIELD", com.sap.mw.jco.JCO.TYPE_CHAR, 30, 60, 1008, 0, null, "FIELD", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SYSTEM", com.sap.mw.jco.JCO.TYPE_CHAR, 10, 20, 1068, 0, null, "SYSTEM", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        this.addStructureDefinitionToCache(smeta);
        
        
/*
ADDITIONAL_INFO                 ,BAPISFLADD                      ,u,15,0,0,Additional Data,EXPORT,BAPISFLADD
AVAILIBILITY                    ,BAPISFLAVA                      ,u,12,15,0,Availability,EXPORT,BAPISFLAVA
FLIGHT_DATA                     ,BAPISFLDAT                      ,u,11,27,0,Flight data,EXPORT,BAPISFLDAT
AIRLINEID                       ,BAPISFLKEY                      ,C,3,38,0,Airline Code,IMPORT,BAPISFLKEY
CONNECTIONID                    ,BAPISFLKEY                      ,N,4,41,0,Flight connection code,IMPORT,BAPISFLKEY
FLIGHTDATE                      ,BAPISFLKEY                      ,D,8,45,0,Departure date,IMPORT,BAPISFLKEY
EXTENSION_IN                    ,BAPIPAREX                       ,h,12,53,0,Import customer enhancements,OPTIONAL,BAPIPAREX
EXTENSION_OUT                   ,BAPIPAREX                       ,h,13,65,0,Export customer enhancements,OPTIONAL,BAPIPAREX
RETURN                          ,BAPIRET2                        ,h,6,78,0,Return Messages,OPTIONAL,BAPIRET2
 */
        
        //------------------------------------------------------------------------------
        //  Add function 'IDOC_INBOUND_ASYNCHRONOUS'
        //------------------------------------------------------------------------------
        fmeta = new JCO.MetaData("IDOC_INBOUND_ASYNCHRONOUS");
        fmeta.addInfo("IDOC_CONTROL_REC_40", com.sap.mw.jco.JCO.TYPE_TABLE, 524, 524, 0, 0, null, "IDOC_CONTROL_REC_40", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, "EDI_DC40", null);
        fmeta.addInfo("IDOC_DATA_REC_40", com.sap.mw.jco.JCO.TYPE_TABLE, 1063, 1063, 0, 0, null, "IDOC_DATA_REC_40", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, "EDI_DD40", null);
        addFunctionInterfaceToCache(fmeta);
        
        //------------------------------------------------------------------------------
        // Add the structure EDI_DC40 to the structure cache
        //------------------------------------------------------------------------------
        smeta =  new com.sap.mw.jco.JCO.MetaData("EDI_DC40");
        smeta.addInfo("TABNAM", com.sap.mw.jco.JCO.TYPE_CHAR, 10, 10, 0, 0, null, "TABNAM", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MANDT", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 3, 10, 0, null, "MANDT", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("DOCNUM", com.sap.mw.jco.JCO.TYPE_CHAR, 16, 16, 13, 0, null, "DOCNUM", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("DOCREL", com.sap.mw.jco.JCO.TYPE_CHAR, 4, 4, 29, 0, null, "DOCREL", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("STATUS", com.sap.mw.jco.JCO.TYPE_CHAR, 2, 2, 33, 0, null, "STATUS", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("DIRECT", com.sap.mw.jco.JCO.TYPE_CHAR, 1, 1, 35, 0, null, "DIRECT", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("OUTMOD", com.sap.mw.jco.JCO.TYPE_CHAR, 1, 1, 36, 0, null, "OUTMOD", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("EXPRSS", com.sap.mw.jco.JCO.TYPE_CHAR, 1, 1, 37, 0, null, "EXPRSS", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("TEST", com.sap.mw.jco.JCO.TYPE_CHAR, 1, 1, 38, 0, null, "TEST", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("IDOCTYP", com.sap.mw.jco.JCO.TYPE_CHAR, 30, 30, 39, 0, null, "IDOCTYP", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("CIMTYP", com.sap.mw.jco.JCO.TYPE_CHAR, 30, 30, 69, 0, null, "CIMTYP", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MESTYP", com.sap.mw.jco.JCO.TYPE_CHAR, 30, 30, 99, 0, null, "MESTYP", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MESCOD", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 3, 129, 0, null, "MESCOD", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MESFCT", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 3, 132, 0, null, "MESFCT", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("STD", com.sap.mw.jco.JCO.TYPE_CHAR, 1, 1, 135, 0, null, "STD", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("STDVRS", com.sap.mw.jco.JCO.TYPE_CHAR, 6, 6, 136, 0, null, "STDVRS", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("STDMES", com.sap.mw.jco.JCO.TYPE_CHAR, 6, 6, 142, 0, null, "STDMES", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SNDPOR", com.sap.mw.jco.JCO.TYPE_CHAR, 10, 10, 148, 0, null, "SNDPOR", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SNDPRT", com.sap.mw.jco.JCO.TYPE_CHAR, 2, 2, 158, 0, null, "SNDPRT", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SNDPFC", com.sap.mw.jco.JCO.TYPE_CHAR, 2, 2, 160, 0, null, "SNDPFC", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SNDPRN", com.sap.mw.jco.JCO.TYPE_CHAR, 10, 10, 162, 0, null, "SNDPRN", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SNDSAD", com.sap.mw.jco.JCO.TYPE_CHAR, 21, 21, 172, 0, null, "SNDSAD", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SNDLAD", com.sap.mw.jco.JCO.TYPE_CHAR, 70, 70, 193, 0, null, "SNDLAD", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("RCVPOR", com.sap.mw.jco.JCO.TYPE_CHAR, 10, 10, 263, 0, null, "RCVPOR", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("RCVPRT", com.sap.mw.jco.JCO.TYPE_CHAR, 2, 2, 273, 0, null, "RCVPRT", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("RCVPFC", com.sap.mw.jco.JCO.TYPE_CHAR, 2, 2, 275, 0, null, "RCVPFC", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("RCVPRN", com.sap.mw.jco.JCO.TYPE_CHAR, 10, 10, 277, 0, null, "RCVPRN", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("RCVSAD", com.sap.mw.jco.JCO.TYPE_CHAR, 21, 21, 287, 0, null, "RCVSAD", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("RCVLAD", com.sap.mw.jco.JCO.TYPE_CHAR, 70, 70, 308, 0, null, "RCVLAD", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("CREDAT", com.sap.mw.jco.JCO.TYPE_DATE, 8, 8, 378, 0, null, "CREDAT", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("CRETIM", com.sap.mw.jco.JCO.TYPE_TIME, 6, 6, 386, 0, null, "CRETIM", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("REFINT", com.sap.mw.jco.JCO.TYPE_CHAR, 14, 14, 392, 0, null, "REFINT", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("REFGRP", com.sap.mw.jco.JCO.TYPE_CHAR, 14, 14, 406, 0, null, "REFGRP", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("REFMES", com.sap.mw.jco.JCO.TYPE_CHAR, 14, 14, 420, 0, null, "REFMES", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("ARCKEY", com.sap.mw.jco.JCO.TYPE_CHAR, 70, 70, 434, 0, null, "ARCKEY", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SERIAL", com.sap.mw.jco.JCO.TYPE_CHAR, 20, 20, 504, 0, null, "SERIAL", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        this.addStructureDefinitionToCache(smeta);
        
        //------------------------------------------------------------------------------
        // Add the structure EDI_DD40 to the structure cache
        //------------------------------------------------------------------------------
        smeta =  new com.sap.mw.jco.JCO.MetaData("EDI_DD40");
        smeta.addInfo("SEGNAM", com.sap.mw.jco.JCO.TYPE_CHAR, 30, 30, 0, 0, null, "SEGNAM", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("MANDT", com.sap.mw.jco.JCO.TYPE_CHAR, 3, 3, 30, 0, null, "MANDT", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("DOCNUM", com.sap.mw.jco.JCO.TYPE_CHAR, 16, 16, 33, 0, null, "DOCNUM", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SEGNUM", com.sap.mw.jco.JCO.TYPE_CHAR, 6, 6, 49, 0, null, "SEGNUM", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("PSGNUM", com.sap.mw.jco.JCO.TYPE_NUM, 6, 6, 55, 0, null, "PSGNUM", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("HLEVEL", com.sap.mw.jco.JCO.TYPE_CHAR, 2, 2, 61, 0, null, "HLEVEL", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        smeta.addInfo("SDATA", com.sap.mw.jco.JCO.TYPE_CHAR, 1000, 1000, 63, 0, null, "SDATA", com.sap.mw.jco.JCO.OPTIONAL_PARAMETER, null, null);
        this.addStructureDefinitionToCache(smeta);
        
        String rfmName = "BAPI_FLIGHT_GETDETAIL";
        IFunctionTemplate ft = this.getFunctionTemplate(rfmName);
        
        /*
        if (ft == null) {
           System.out.println("JUnit Unable to find SAP RFM " + rfmName);
        } else {
            System.out.println("FOUND SAP RFM " + rfmName);
        }
         */
   }
}
