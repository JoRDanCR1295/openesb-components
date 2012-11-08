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
 * @(#)BPELHelperTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.net.URI;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.extensions.schema.Schema;

import junit.framework.TestCase;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.ProjectBasedWSDLResolver;
import com.sun.bpel.model.ProjectBasedWSDLResolverFactory;
import com.sun.bpel.model.ProjectBasedXSDResolver;
import com.sun.bpel.model.ProjectBasedXSDResolverFactory;
import com.sun.bpel.model.Receive;
import com.sun.bpel.model.Reply;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class BPELHelperTest extends TestCase {

	protected void setUp() throws Exception {
		// TODO Auto-generated method stub
		super.setUp();
	}

	protected void tearDown() throws Exception {
		// TODO Auto-generated method stub
		super.tearDown();
	}

	public void testNullPartnerLink() {
		try {
            String fileName = "/com/sun/bpel/model/bpelHelperTest/echo.bpel";
            URL url = getClass().getResource(fileName);
            URI uri = url.toURI();
             
            String baseURI = uri.toString();
            
            BPELParseContext parseContext = new BPELParseContext.DefaultParseContext();
            ProjectBasedWSDLResolver loader = ProjectBasedWSDLResolverFactory.getInstance().newWSDLResolver(baseURI, parseContext);
            parseContext.setWSDLResolver(loader);
            
            ProjectBasedXSDResolver xsdResolver = ProjectBasedXSDResolverFactory.getInstance().newXSDResolver(baseURI, parseContext);
            parseContext.setXSDResolver(xsdResolver);
            
            Reader reader = new FileReader(new File(uri));

            ParsingCaches caches = new ParsingCaches();
            parseContext.setCaches(caches);
            DeferredActionRegistry registry = new DeferredActionRegistry();
            parseContext.setDeferredActionRegistry(registry);
            BPELDocument document = BPELDocumentParseFactory.getInstance().load(reader, parseContext);
            WSDL4JExt.applySingleSchemaTypeLoader(registry, parseContext.getBaseURIResolver());
            
            assertNotNull(document);
            
            BPELProcess process = document.getDocumentProcess();
            assertNotNull(process);
            
            Activity activity = process.getActivity();
            
            //top level activity is a sequence
            assertTrue(activity instanceof Sequence);
            
            Sequence sequence = (Sequence) activity;
            
            //sequence has one scope activity
            Collection activitites = sequence.getActivities();
            assertEquals(1, activitites.size());
            
            Iterator it = activitites.iterator();
            
            Object act =  it.next();
            //activity inside sequence is a scope
            assertTrue(act instanceof Scope);
            
            Scope scope = (Scope) act;
            
            //now check for scoped variable
            Variables variables = scope.getVariables();
            //variables should not be null
            assertNotNull(variables);
            
            //there should be two variables
            Collection varCollection = variables.getVariables();
            assertTrue(varCollection.size() == 2);
            
            //make sure variables are correct
            it = varCollection.iterator();
            while(it.hasNext()) {
            	Variable variable = (Variable) it.next();
            	
            	assertNotNull(variable.getName());
            	assertNotNull(variable.getMessageType());
            	assertNotNull(variable.getWSDLMessageType());
            }
            
            //there should be one top level sequence inside scope
            Activity actInsideScope = scope.getActivity();
            assertTrue(actInsideScope instanceof Sequence);
            
            Sequence sequenceInsideScope = (Sequence) actInsideScope;
            
//          BPELHelper Api test
    		//check if bogus partnerLink is null
    		PartnerLink partnerLink  = BPELHelper.getMatchingPartnerLink("bogus", sequenceInsideScope);
    		assertNull(partnerLink);
    		
            
		} catch(Exception ex) {
            fail(ex.getMessage());
        }
    
	}
	
	public void testNullVariable() {
		try {
            String fileName = "/com/sun/bpel/model/bpelHelperTest/echo.bpel";
            URL url = getClass().getResource(fileName);
            URI uri = url.toURI();
             
            String baseURI = uri.toString();
            
            BPELParseContext parseContext = new BPELParseContext.DefaultParseContext();
            ProjectBasedWSDLResolver loader = ProjectBasedWSDLResolverFactory.getInstance().newWSDLResolver(baseURI, parseContext);
            parseContext.setWSDLResolver(loader);
            
            ProjectBasedXSDResolver xsdResolver = ProjectBasedXSDResolverFactory.getInstance().newXSDResolver(baseURI, parseContext);
            parseContext.setXSDResolver(xsdResolver);
            
            Reader reader = new FileReader(new File(uri));
            
            ParsingCaches caches = new ParsingCaches();
            parseContext.setCaches(caches);
            DeferredActionRegistry registry = new DeferredActionRegistry();
            parseContext.setDeferredActionRegistry(registry);
            BPELDocument document = BPELDocumentParseFactory.getInstance().load(reader, parseContext);
            WSDL4JExt.applySingleSchemaTypeLoader(registry, parseContext.getBaseURIResolver());
            assertNotNull(document);
            
            BPELProcess process = document.getDocumentProcess();
            assertNotNull(process);
            
            Activity activity = process.getActivity();
            
            //top level activity is a sequence
            assertTrue(activity instanceof Sequence);
            
            Sequence sequence = (Sequence) activity;
            
            //sequence has one scope activity
            Collection activitites = sequence.getActivities();
            assertEquals(1, activitites.size());
            
            Iterator it = activitites.iterator();
            
            Object act =  it.next();
            //activity inside sequence is a scope
            assertTrue(act instanceof Scope);
            
            Scope scope = (Scope) act;
            
            //now check for scoped variable
            Variables variables = scope.getVariables();
            //variables should not be null
            assertNotNull(variables);
            
            //there should be two variables
            Collection varCollection = variables.getVariables();
            assertTrue(varCollection.size() == 2);
            
            //make sure variables are correct
            it = varCollection.iterator();
            while(it.hasNext()) {
            	Variable variable = (Variable) it.next();
            	
            	assertNotNull(variable.getName());
            	assertNotNull(variable.getMessageType());
            	assertNotNull(variable.getWSDLMessageType());
            }
            
            //there should be one top level sequence inside scope
            Activity actInsideScope = scope.getActivity();
            assertTrue(actInsideScope instanceof Sequence);
            
            Sequence sequenceInsideScope = (Sequence) actInsideScope;
            
//          BPELHelper Api test
    		//check if bogus variable is null
    		Variable variable = BPELHelper.getMatchingVariable("bogus", sequenceInsideScope);
    		assertNull(variable);
    		
            
		} catch(Exception ex) {
            fail(ex.getMessage());
        }
	}
}
