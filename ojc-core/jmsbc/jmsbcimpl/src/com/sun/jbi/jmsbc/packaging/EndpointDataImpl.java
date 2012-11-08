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
 * @(#)EndpointDataImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.packaging;

import java.io.InputStream;
import java.io.IOException;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.SAXException;
import org.xml.sax.InputSource;

/**
 * Class which represents the "raw" endpoint 
 * information independent of the format of the endpoint configuration file
 * 
 */
public class EndpointDataImpl implements EndpointData  {
    
    private String mInterface;
    private String mService;
    private String mEndPoint;
    private int mDirection;
    private String mConfig;

    protected EndpointDataImpl(String interfaceName,
                               String service,
                               String endPoint,
                               int direction,
                               String config) {
        mInterface = interfaceName;
        mService = service;
        mEndPoint = endPoint;
        mDirection = direction;
        mConfig = config;
    }

    public String getInterface() {
        return mInterface;
    }
    
    public String getService() {
        return mService;
    }

    public String getEndpoint() {
        return mEndPoint;
    }

    public int getDirection() {
        return mDirection;
    }

	public String getApplicationConfigurationName() {
		return mConfig;
	}

}
