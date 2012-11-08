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
 * @(#)PackagingTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.packaging;


import junit.framework.*;

import java.io.File;
import java.util.List;
import java.util.Collection;
import java.util.Iterator;
import javax.xml.namespace.QName;
import com.sun.jbi.smtpbc.Endpoint;
import com.sun.jbi.smtpbc.Endpoint.EndpointType;
import com.sun.jbi.smtpbc.packaging.EndpointConfiguration.PortMap;
import java.util.HashMap;


/**
 * * *
 * @author rchen
 */
public class PackagingTest extends TestCase
{

    EndpointConfiguration endpointConfig = null;

    WSDLConfigurations wsdlConfig = null;

    Collection mEndpoints = null;

    public PackagingTest( final String testName )
    {
        super( testName );
    }

    @Override
	protected void setUp()
        throws Exception
    {
        final String rootDir = new File("test/com/sun/jbi/smtpbc/packaging").getAbsolutePath();
        endpointConfig = EndpointConfiguration.newConfiguration( rootDir );
        wsdlConfig = new WSDLConfigurations( rootDir );
        HashMap envVarMap = new HashMap();
        mEndpoints = wsdlConfig.parse( endpointConfig.portMaps(),envVarMap );
    }

    @Override
	protected void tearDown()
        throws Exception
    {}

    /**
     * Test of newConfiguration method, of class com.sun.jbi.smtpbc.packaging.EndpointConfiguration.
     */
    public void testNewConfiguration()
        throws Exception
    {
        System.out.println( "newConfiguration" );
        Assert.assertNotNull( endpointConfig );
        Assert.assertNotNull( wsdlConfig );
        Assert.assertNotNull( mEndpoints );
    }

    /**
     * Test of addPortMap method, of class com.sun.jbi.smtpbc.packaging.EndpointConfiguration.
     */
    public void testAddPortMap()
    {
        System.out.println( "addPortMap" );
        // EndpointConfiguration.PortMap p = null;
        // EndpointConfiguration instance = null;
        // instance.addPortMap(p);
        // TODO review the generated test code and remove the default call to fail.
        // fail("The test case is a prototype.");
    }

    /**
     * Test of portMaps method, of class com.sun.jbi.smtpbc.packaging.EndpointConfiguration.
     */
    public void testPortMaps()
    {
        System.out.println( "testPortMaps" );
        final List result = endpointConfig.portMaps();
        Assert.assertEquals( 5, result.size() );
        final Iterator it = result.iterator();
        while (it.hasNext()) {
            final PortMap portMap = (PortMap) it.next();
            final String service = portMap.getService();
            final String endPoint = portMap.getEndpoint();
            final EndpointType endPointType = portMap.getDirection();
            String endpttype = "";
            if (endPointType == EndpointType.INBOUND) {
                endpttype = "EndpointType.INBOUND";
            } else if (endPointType == EndpointType.OUTBOUND) {
                endpttype = "EndpointType.OUTBOUND";
            } else {
                Assert.fail( "Wrong end point type" );
            }
            System.out.println( "portmap service:" + service + "  :endpoint:" + endPoint + "  :endpointtype:" + endpttype );
        }
    }

    public void testEndPoints()
    {
        System.out.println( "testEndPoints" );
        Assert.assertEquals( 5, mEndpoints.size() );
        final Iterator it = mEndpoints.iterator();
        while (it.hasNext()) {
            final Endpoint endpoint = (Endpoint) it.next();
            // Store the status...
            final QName serviceName = endpoint.getServiceName();
            final String portName = endpoint.getEndpointName();
            System.out.println( "Endpoint serviceName:" + serviceName + "   :portName:" + portName );
        }
    }

}
