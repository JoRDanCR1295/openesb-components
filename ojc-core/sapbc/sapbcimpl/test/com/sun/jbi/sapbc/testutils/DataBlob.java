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
 * @(#)DataBlob.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.testutils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

/**
 * Utility to hold data for the test units.
 *
 * @author Julie Knight (julie.knight@sun.com)
 */
public class DataBlob {
    
    /**
     * Constructor
     *@param dataFile Absolute path pointing to the file that contains information for the object
     */
    public DataBlob(File dataFile) 
    throws java.lang.Exception {
        parseDataFile(dataFile);
    }
    
    /**
     * Loads and parses the data file
     * Expecting three lines:
     * 1) The BAPI NAME
     * 2) The JCO data file name - formate JCORequests\filename or JCOResponses\filename
     * 3) The XML data file name - formate XMLInput\filename or XMLOutput\filename
     *
     *@param dataFile Absolute path pointing to the file that contains information for the object
     */
    void parseDataFile(File dataFile) 
    throws java.io.FileNotFoundException, java.io.IOException, java.lang.Exception {
      BufferedReader fin = new BufferedReader(new FileReader(dataFile));
      String bapiName = fin.readLine();
      String jcoFile = fin.readLine();
      String xmlFile = fin.readLine();
      
      if (bapiName == null) {
        throw new Exception("No data found for BAPI name. File name ["+dataFile+"]");  
      } else if (jcoFile == null) {
        throw new Exception("No file name found for JCO data. File name ["+dataFile+"]");  
      } else if (xmlFile == null) {
        throw new Exception("No file name found for XML data. File name ["+dataFile+"]");  
      }
      setBapiName(bapiName);
      setJCOData(readFile(jcoFile));
      setXMLData(readFile(xmlFile));
    }
    
    /**
     * Holda BAPI name
     *@param bapiName Name of the BAPI for this blob
     */
    void setBapiName(String bapiName) {
      mBapiName = bapiName;  
    }
    /**
     * Retrieves BAPI name
     *@returns The name of the BAPI for this blob
     */
    public String getBapiName() {
      return mBapiName;
    }
    
    /**
     * Holda JCO Data
     *@param jcoData The JCO data stored; either JCO Request or Response
     */
    void setJCOData(String jcoData) {
      mJCOData = jcoData;  
    }
    /**
     * Retrieves BAPI name
     *@returns The JCO data stored; either JCO Request or Response
     */
    public String getJCOData() {
      return mJCOData;
    }
    
    /**
     * Holda XML data
     *@param xmlData The XML data stored; based on XML from the NMR
     */
    void setXMLData(String xmlData) {
      mXMLData = xmlData;  
    }
    /**
     * Retrieves XML data
     *@returns The XML data stored; based on XML from the NMR
     */
    public String getXMLData() {
      return mXMLData;
    }
    
    /**
     * Loads a file and reads in the data
     *@param dataFile Absolute path pointing to the file that contains information for the object
     */
    String readFile(String file) 
    throws java.io.FileNotFoundException, java.io.IOException {
        FileReader is = new FileReader(file);
        StringBuffer sb = new StringBuffer();
        char[] b = new char[8192];
        int n;
        while ((n = is.read(b)) > 0) {
            sb.append(b, 0, n);
        }
        return sb.toString();
    }
    
    String mBapiName = null;
    String mJCOData = null;
    String mXMLData = null;
}
