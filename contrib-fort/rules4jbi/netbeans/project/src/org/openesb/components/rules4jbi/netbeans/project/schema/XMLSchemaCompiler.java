/*
 * @(#)XMLSchemaCompiler.java        $Revision: 1.2 $ $Date: 2008/11/04 18:48:13 $
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

package org.openesb.components.rules4jbi.netbeans.project.schema;

import java.io.File;
import java.util.Iterator;
import java.util.logging.Logger;

import org.xml.sax.InputSource;
import org.xml.sax.SAXParseException;

import com.sun.tools.xjc.api.ErrorListener;
import com.sun.tools.xjc.api.SchemaCompiler;
import com.sun.tools.xjc.api.S2JJAXBModel;
import com.sun.tools.xjc.api.XJC;
import com.sun.codemodel.JClass;
import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JPackage;


/**
 *
 * @author Prem Kumar
 * @version $Revision: 1.2 $ $Date: 2008/11/04 18:48:13 $
 * 
 * @since 0.2
 */
public class XMLSchemaCompiler {

    private static final Logger logger = Logger.getLogger(XMLSchemaCompiler.class.getName());

    private SchemaCompiler mSchemaCompiler;
    
    private String mDestDir;

    public XMLSchemaCompiler() {
        initialize();
    }

    /**
     * Initializes the options
     *
     */
    private void initialize() {
        logger.fine("Initializing schema compiler");
        mSchemaCompiler = XJC.createSchemaCompiler();
        mSchemaCompiler.setErrorListener(new MyErrorListener());
    }

    /**
     * Sets the schema for which java classes are generated
     * @param schema
     */
    public void parseSchema(String schema) {
        logger.fine("Parsing schema");
        mSchemaCompiler.parseSchema(new InputSource(schema));
    }

    /**
     * Sets the directory where the generated files will go
     * @param destDir
     */
    public void setDestDir(String destDir) {
        mDestDir = destDir;
    }

    /**
     * Get the generated source dir 
     * @return
     */
    public String getGeneratedSourceDir() {
        return mDestDir;
    }

    /**
     * Invokes JAXB XJC tool
     * @throws Exception
     */
    public void invokeXJC() throws Exception {
        logger.fine("Creating the model to generate business objects");
        S2JJAXBModel model = mSchemaCompiler.bind();
        JCodeModel jcmodel = model.generateCode(null, null);
        File file = new File(mDestDir);
        if (!file.exists()) {
            file.mkdir();
        }
        
        // delete the generated ObjectFactory.java, as it is unnecessary
        Iterator pkgIterator = jcmodel.packages();
        while(pkgIterator.hasNext()){
            JPackage pkg = (JPackage) pkgIterator.next();
            if(pkg.isDefined("ObjectFactory")){
                JDefinedClass objFactoryClass = pkg._getClass("ObjectFactory");
                pkg.remove(objFactoryClass);
            }
        }
        
        jcmodel.build(file);
                        
    }

    class MyErrorListener implements ErrorListener {

        MyErrorListener() {
        }

        public void error(SAXParseException exception) {
            logger.severe("XMLSchemaCompiler cannot parse xsd :" + exception.getMessage());
        }

        public void fatalError(SAXParseException exception) {
            logger.severe("XMLSchemaCompiler encountered a fatal error :" + exception.getMessage());
        }

        public void warning(SAXParseException exception) {
            logger.severe("XMLSchemaCompiler warning :" + exception.getMessage());
        }

        public void info(SAXParseException exception) {
            logger.severe("XMLSchemaCompiler info :" + exception.getMessage());
        }
    }
}
