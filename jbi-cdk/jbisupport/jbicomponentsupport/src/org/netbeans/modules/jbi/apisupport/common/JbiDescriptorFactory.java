/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 *
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.jbi.apisupport.common;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.w3c.dom.Document;

/**
 *
 * @author chikkala
 */
public class JbiDescriptorFactory {
    
    private static JbiDescriptorFactory sFactory = null;
    /** Creates a new instance of JbiDescriptorFactory */
    protected JbiDescriptorFactory() {
    }
    
    public static JbiDescriptorFactory getInstance() {
        if ( sFactory == null ){
            sFactory = new JbiDescriptorFactory();
        }
        return sFactory;
    }
    
    public JbiComponentDescriptor createJbiComponentDescriptor(Reader jbiXmlReader) {
        
        JbiComponentDescriptor compDescriptor = null;
        Document domDoc = null;
        try {
            domDoc = DOMUtil.UTIL.buildDOMDocument(jbiXmlReader, false);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        if ( domDoc != null ) {
            compDescriptor = new JbiComponentDescriptorImpl(domDoc);
        }
        return compDescriptor;
    }
    
    public JbiComponentDescriptor createServiceEngineDescriptor(Reader jbiXmlReader) {
        
        JbiComponentDescriptor seDescriptor = createJbiComponentDescriptor(jbiXmlReader);
        
        if ( JbiDescriptor.SERVICE_ENGINE_TYPE.equals(seDescriptor.getType()) ) {
            return seDescriptor;
        } else {
            return null;
        }
    }
    
    public JbiComponentDescriptor createBindingComponentDescriptor(Reader jbiXmlReader) {
        
        JbiComponentDescriptor bcDescriptor = createJbiComponentDescriptor(jbiXmlReader);
        if ( JbiDescriptor.BINDING_COMPONENT_TYPE.equals(bcDescriptor.getType()) ) {
            return bcDescriptor;
        } else {
            return null;
        }
    }
    
    public JbiComponentDescriptor createJbiComponentDescriptor(FileObject jbiXmlFO) {
        JbiComponentDescriptor compDesc = null;
        InputStream inS = null;
        InputStreamReader reader = null;
        try {
            inS = jbiXmlFO.getInputStream();
            reader = new InputStreamReader(inS);
            compDesc = createJbiComponentDescriptor(reader);
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } finally {
            TemplateUtil.closeReader(reader);
            TemplateUtil.closeInputStream(inS);
        }
        return compDesc;
    }
    
    public void saveJbiDescriptor(JbiDescriptor jbiDescriptor, FileObject outFO) {
        
        FileLock outLock = null;
        OutputStream outS = null;
        OutputStreamWriter outWriter = null;
        Document domDoc = null;
        try {
            domDoc = jbiDescriptor.getDOMDocument();
            outLock = outFO.lock();
            outS = outFO.getOutputStream(outLock);
            outWriter = new OutputStreamWriter(outS);
            DOMUtil.UTIL.DOM2Text(domDoc, outWriter);
        } catch ( Exception ex) {
            ex.printStackTrace();
        } finally {
            TemplateUtil.closeOutputStream(outS);
            TemplateUtil.closeWriter(outWriter);
            if ( outLock != null ) {
                outLock.releaseLock();
            }
        }
    }
    
}
