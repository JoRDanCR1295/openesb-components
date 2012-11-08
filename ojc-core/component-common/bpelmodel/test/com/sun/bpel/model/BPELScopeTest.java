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
 * @(#)BPELScopeTest.java 
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

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.PartnerLinks;
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

import junit.framework.TestCase;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class BPELScopeTest extends TestCase {

	protected void setUp() throws Exception {
		// TODO Auto-generated method stub
		super.setUp();
	}

	protected void tearDown() throws Exception {
		// TODO Auto-generated method stub
		super.tearDown();
	}

	public void testScopedVariables() {
		try {
            String fileName = "/com/sun/bpel/model/bpelScopeTest/echo.bpel";
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
            
            //this sequence inside scope should have 3 activities
            //receive-->assign-->reply
            Collection activites = sequenceInsideScope.getActivities();
            assertTrue(activites.size() == 3);
            
            //first child of sequence should be a receive
            it = activites.iterator();
            int counter = 1;
            while(it.hasNext()) {
            	Object seqActivity = it.next();
            	switch(counter) {
	            	case 1:
	            		assertTrue(seqActivity instanceof Receive);
	            		Receive receive = (Receive) seqActivity;
	            		
	            		//make sure receive is valid
	            		assertNotNull(receive.getName());
	            		assertNotNull(receive.getPartnerLink());
	            		assertNotNull(receive.getBPELPartnerLink());
	            		assertNotNull(receive.getPortType());
	            		assertNotNull(receive.getWSDLPortType());
	            		assertNotNull(receive.getOperation());
	            		assertNotNull(receive.getWSDLOperation());
	            		assertNotNull(receive.getVariable());
	            		assertNotNull(receive.getBPELVariable());
	            		
	            		//make sure that variable is the one which is 
	            		//defined in scope not at parent process level
	            		Variable variable = receive.getBPELVariable();
	            		XMLNode variablesNode = variable.getParent();
	            		XMLNode scopeNode = variablesNode.getParent();
	            		assertTrue(scopeNode.equals(scope));
	            		break;
	            	case 2:
	            		assertTrue(seqActivity instanceof Assign);
	            		Assign assign = (Assign) seqActivity;
	            		break;
	            	case 3:
	            		assertTrue(seqActivity instanceof Reply);
	            		Reply reply = (Reply) seqActivity;
	            		
	            		assertNotNull(reply.getName());
	            		assertNotNull(reply.getPartnerLink());
	            		assertNotNull(reply.getBPELPartnerLink());
	            		assertNotNull(reply.getPortType());
	            		assertNotNull(reply.getWSDLPortType());
	            		assertNotNull(reply.getOperation());
	            		assertNotNull(reply.getWSDLOperation());
	            		assertNotNull(reply.getVariable());
	            		assertNotNull(reply.getBPELVariable());
	            		
	            		//make sure that variable is the one which is 
	            		//defined in scope not at parent process level
	            		variable = reply.getBPELVariable();
	            		variablesNode = variable.getParent();
	            		scopeNode = variablesNode.getParent();
	            		assertTrue(scopeNode.equals(scope));
	            		break;
	            	}
            	counter++;
            }
            
		} catch(Exception ex) {
            fail(ex.getMessage());
        }
        
	}
	
	public void testScopedPartnerLinks() {
		try {
            String fileName = "/com/sun/bpel/model/bpelScopeTest/echo.bpel";
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
            
            //now check for scoped partnerLinks
            PartnerLinks partnerLinks = scope.getPartnerLinks();
            //partnerLinks should not be null
            assertNotNull(partnerLinks);
            
            //there should be one partnerLinks
            Collection partnerLinkCollection = partnerLinks.getPartnerLinks();
            assertTrue(partnerLinkCollection.size() == 1);
            
            //make sure partnerLinks are correct
            it = partnerLinkCollection.iterator();
            while(it.hasNext()) {
            	PartnerLink partner = (PartnerLink) it.next();
            	
            	assertNotNull(partner.getName());
            	assertNotNull(partner.getPartnerLinkType());
            	assertNotNull(partner.getBPELPartnerLinkType());
            	assertNotNull(partner.getMyRole());
            }
            
            //there should be one top level sequence inside scope
            Activity actInsideScope = scope.getActivity();
            assertTrue(actInsideScope instanceof Sequence);
            
            Sequence sequenceInsideScope = (Sequence) actInsideScope;
            
            //this sequence inside scope should have 3 activities
            //receive-->assign-->reply
            Collection activites = sequenceInsideScope.getActivities();
            assertTrue(activites.size() == 3);
            
            //first child of sequence should be a receive
            it = activites.iterator();
            int counter = 1;
            while(it.hasNext()) {
            	Object seqActivity = it.next();
            	switch(counter) {
	            	case 1:
	            		assertTrue(seqActivity instanceof Receive);
	            		Receive receive = (Receive) seqActivity;
	            		
	            		//make sure receive is valid
	            		assertNotNull(receive.getName());
	            		assertNotNull(receive.getPartnerLink());
	            		assertNotNull(receive.getBPELPartnerLink());
	            		assertNotNull(receive.getPortType());
	            		assertNotNull(receive.getWSDLPortType());
	            		assertNotNull(receive.getOperation());
	            		assertNotNull(receive.getWSDLOperation());
	            		assertNotNull(receive.getVariable());
	            		assertNotNull(receive.getBPELVariable());
	            		
	            		//make sure that partnerLink is the one which is 
	            		//defined in scope not at parent process level
	            		PartnerLink partner = receive.getBPELPartnerLink();
	            		XMLNode partnerLinksNode = partner.getParent();
	            		XMLNode scopeNode = partnerLinksNode.getParent();
	            		assertTrue(scopeNode.equals(scope));
	            		break;
	            	case 2:
	            		assertTrue(seqActivity instanceof Assign);
	            		Assign assign = (Assign) seqActivity;
	            		break;
	            	case 3:
	            		assertTrue(seqActivity instanceof Reply);
	            		Reply reply = (Reply) seqActivity;
	            		
	            		assertNotNull(reply.getName());
	            		assertNotNull(reply.getPartnerLink());
	            		assertNotNull(reply.getBPELPartnerLink());
	            		assertNotNull(reply.getPortType());
	            		assertNotNull(reply.getWSDLPortType());
	            		assertNotNull(reply.getOperation());
	            		assertNotNull(reply.getWSDLOperation());
	            		assertNotNull(reply.getVariable());
	            		assertNotNull(reply.getBPELVariable());
	            		
	            		//make sure that variable is the one which is 
	            		//defined in scope not at parent process level
	            		partner = reply.getBPELPartnerLink();
	            		partnerLinksNode = partner.getParent();
	            		scopeNode = partnerLinksNode.getParent();
	            		assertTrue(scopeNode.equals(scope));
	            		break;
	            	}
            	counter++;
            }
            
		} catch(Exception ex) {
            fail(ex.getMessage());
        }
	}
}
