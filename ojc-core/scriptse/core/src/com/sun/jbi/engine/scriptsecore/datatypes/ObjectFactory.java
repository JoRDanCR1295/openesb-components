//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vJAXB 2.0 in JDK 1.6 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.03.28 at 08:23:26 PM IST 
//
package com.sun.jbi.engine.scriptsecore.datatypes;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * This object contains factory methods for each  Java content interface and Java element
 * interface  generated in the com.sun.jbi.engine.scriptse.datatypes package.<p>An
 * ObjectFactory allows you to programatically  construct new instances of the Java representation
 * for XML content. The Java representation of XML  content can consist of schema derived
 * interfaces  and classes representing the binding of schema  type definitions, element
 * declarations and model  groups.  Factory methods for each of these are  provided in this class.</p>
 */
@XmlRegistry
public class ObjectFactory {

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema
     * derived classes for package: com.sun.jbi.engine.scriptse.datatypes
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Script.ScriptSource.File }
     *
     * @return DOCUMENT ME!
     */
    public Script.ScriptSource.File createScriptScriptSourceFile() {
        return new Script.ScriptSource.File();
    }

    /**
     * Create an instance of {@link Script.ScriptType.ScriptEngine.Language }
     *
     * @return DOCUMENT ME!
     */
    public Script.ScriptType.ScriptEngine.Language createScriptScriptTypeScriptEngineLanguage() {
        return new Script.ScriptType.ScriptEngine.Language();
    }

    /**
     * Create an instance of {@link Script.InputParameters.Param }
     *
     * @return DOCUMENT ME!
     */
    public Script.InputParameters.Param createScriptInputParametersParam() {
        return new Script.InputParameters.Param();
    }

    /**
     * Create an instance of {@link Script }
     *
     * @return DOCUMENT ME!
     */
    public Script createScript() {
        return new Script();
    }

    /**
     * Create an instance of {@link Script.ScriptSource.Url }
     *
     * @return DOCUMENT ME!
     */
    public Script.ScriptSource.Url createScriptScriptSourceUrl() {
        return new Script.ScriptSource.Url();
    }

    /**
     * Create an instance of {@link Script.Result.Fault }
     *
     * @return DOCUMENT ME!
     */
    public Script.Result.Fault createScriptResultFault() {
        return new Script.Result.Fault();
    }

    /**
     * Create an instance of {@link Script.ScriptSource }
     *
     * @return DOCUMENT ME!
     */
    public Script.ScriptSource createScriptScriptSource() {
        return new Script.ScriptSource();
    }

    /**
     * Create an instance of {@link Script.InputParameters }
     *
     * @return DOCUMENT ME!
     */
    public Script.InputParameters createScriptInputParameters() {
        return new Script.InputParameters();
    }

    /**
     * Create an instance of {@link Script.ScriptSource.Inlined }
     *
     * @return DOCUMENT ME!
     */
    public Script.ScriptSource.Inlined createScriptScriptSourceInlined() {
        return new Script.ScriptSource.Inlined();
    }

    /**
     * Create an instance of {@link Script.Result }
     *
     * @return DOCUMENT ME!
     */
    public Script.Result createScriptResult() {
        return new Script.Result();
    }

    /**
     * Create an instance of {@link Script.ScriptType }
     *
     * @return DOCUMENT ME!
     */
    public Script.ScriptType createScriptScriptType() {
        return new Script.ScriptType();
    }

    /**
     * Create an instance of {@link Script.ScriptType.ScriptEngine }
     *
     * @return DOCUMENT ME!
     */
    public Script.ScriptType.ScriptEngine createScriptScriptTypeScriptEngine() {
        return new Script.ScriptType.ScriptEngine();
    }

    /**
     * Create an instance of {@link Script.Result.Resultdata }
     *
     * @return DOCUMENT ME!
     */
    public Script.Result.Resultdata createScriptResultResultdata() {
        return new Script.Result.Resultdata();
    }
}