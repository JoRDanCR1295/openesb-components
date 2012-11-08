/*
 * @(#)JBIUtilsTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.util;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
//import static nu.xom.tests.XOMTestCase.*;
import org.w3c.dom.DocumentFragment;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
 * 
 * @since 0.1
 */
public class JBIUtilsTest {

    private Alpha alpha;
    
    private Beta beta;
    
    private Gamma gamma;
    
    private Delta delta;
    
    private Kappa kappa;
    
    private Sigma sigma;
    
    private Omega omega;
    
    public JBIUtilsTest() {
    }

    @Before
    public void setUp() {
        alpha = new Alpha();
        beta = new Beta();
        gamma = new Gamma();
        delta = new Delta();
        kappa = new Kappa();
        sigma = new Sigma();
        omega = new Omega();
    }

    @Test(expected=NullPointerException.class)
    public void equalNPE1() {
        JBIUtils.equal(alpha, null);
    }

    @Test(expected = NullPointerException.class)
    public void equalNPE2() {
        JBIUtils.equal(null, gamma);
    }

    @Test(expected = NullPointerException.class)
    public void equalNPE3() {
        JBIUtils.equal(null, null);
    }
    
    @Test
    public void equal() {
        assertTrue(JBIUtils.equal(alpha, alpha));
        assertTrue(JBIUtils.equal(delta, delta));
        assertTrue(JBIUtils.equal(kappa, kappa));
        assertTrue(JBIUtils.equal(sigma, sigma));
        
        assertTrue(JBIUtils.equal(alpha, beta));
        assertTrue(JBIUtils.equal(beta, alpha));
        
        assertFalse(JBIUtils.equal(alpha, gamma));
        assertFalse(JBIUtils.equal(gamma, alpha));
        
        assertFalse(JBIUtils.equal(alpha, delta));
        assertFalse(JBIUtils.equal(delta, alpha));
        
        /* we do not compare interfaces anymore */
        assertTrue(JBIUtils.equal(alpha, kappa));
        assertTrue(JBIUtils.equal(kappa, alpha));
        
        assertTrue(JBIUtils.equal(alpha, sigma));
        assertTrue(JBIUtils.equal(sigma, alpha));

        
        assertFalse(JBIUtils.equal(alpha, omega));
        assertFalse(JBIUtils.equal(omega, alpha));
        
        assertFalse(JBIUtils.equal(gamma, delta));
        assertFalse(JBIUtils.equal(delta, gamma));

        /* we do not compare interfaces anymore */
        assertTrue(JBIUtils.equal(kappa, sigma));
        assertTrue(JBIUtils.equal(sigma, kappa));
    }
}

class Alpha implements ServiceEndpoint {

    public DocumentFragment getAsReference(QName operationName) {
        return null;
    }

    public String getEndpointName() {
        return "qqqqq";
    }

    public QName[] getInterfaces() {
        return new QName[] {new QName("aaa", "bbb")};
    }

    public QName getServiceName() {
        return new QName("aaa", "ccc");
    }
}

/* same as Alpha */
class Beta implements ServiceEndpoint {

    public DocumentFragment getAsReference(QName operationName) {
        return null;
    }

    public String getEndpointName() {
        return "qqqqq";
    }

    public QName[] getInterfaces() {
        return new QName[] {new QName("aaa", "bbb")};
    }

    public QName getServiceName() {
        return new QName("aaa", "ccc");
    }
}

/* different endpoint name from Alpha */
class Gamma implements ServiceEndpoint {

    public DocumentFragment getAsReference(QName operationName) {
        return null;
    }

    public String getEndpointName() {
        return "hhhh";
    }

    public QName[] getInterfaces() {
        return new QName[] {new QName("aaa", "bbb")};
    }

    public QName getServiceName() {
        return new QName("aaa", "ccc");
    }
}

/* different service name from Alpha */
class Delta implements ServiceEndpoint {

    public DocumentFragment getAsReference(QName operationName) {
        return null;
    }

    public String getEndpointName() {
        return "qqqqq";
    }

    public QName[] getInterfaces() {
        return new QName[] {new QName("aaa", "bbb")};
    }

    public QName getServiceName() {
        return new QName("aaa", "dddddddddddddd");
    }
}

/* different interface name from Alpha */
class Kappa implements ServiceEndpoint {

    public DocumentFragment getAsReference(QName operationName) {
        return null;
    }

    public String getEndpointName() {
        return "qqqqq";
    }

    public QName[] getInterfaces() {
        return new QName[] {new QName("eeeeeeeeeeeee", "bbb")};
    }

    public QName getServiceName() {
        return new QName("aaa", "ccc");
    }
}

/* different interface count from Alpha */
class Sigma implements ServiceEndpoint {

    public DocumentFragment getAsReference(QName operationName) {
        return null;
    }

    public String getEndpointName() {
        return "qqqqq";
    }

    public QName[] getInterfaces() {
        return new QName[] {new QName("aaa", "bbb"), new QName("aaa", "bbb")};
    }

    public QName getServiceName() {
        return new QName("aaa", "ccc");
    }
}

/* everything is different from Alpha */
class Omega implements ServiceEndpoint {

    public DocumentFragment getAsReference(QName operationName) {
        return null;
    }

    public String getEndpointName() {
        return "omega1";
    }

    public QName[] getInterfaces() {
        return new QName[] {new QName("omega2", "omega3")};
    }

    public QName getServiceName() {
        return new QName("omega4", "omega5");
    }
}
