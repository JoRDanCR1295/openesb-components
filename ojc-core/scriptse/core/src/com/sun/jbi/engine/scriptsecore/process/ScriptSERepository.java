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
 * @(#)ScriptSERepository.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.scriptsecore.process;

import java.util.Hashtable;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptEngineManager;


public class ScriptSERepository {

    private static ScriptSERepository msRep = null;
    private static Logger msLogger = Logger.getLogger(ScriptSERepository.class.getName());
    private static Hashtable mHshTbl = new Hashtable();

    // Defining the constants here
    /** DOCUMENT ME! */
    public static String SCRIPT_ENGINE_FACT = "ScriptEngineFact";
    /** DOCUMENT ME! */
    public static String SCRIPT_ENGINE_MGR = "ScriptEngineMgr";

    //HashTable used to store the contents.
    /**
     * Creates a new instance of ScriptSERepository
     */
    private ScriptSERepository() {
        init();
    } // ScriptSERepository() ends.


    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static ScriptSERepository GetInstance() {
        synchronized (ScriptSERepository.class) {
            if (msRep == null) {
                msRep = new ScriptSERepository();
            }

            return msRep;
        } //synchronized ends.

    } //GetInstance() method ends.

    //  public void store(String aKey, Object aValue)

    /**
     * DOCUMENT ME!
     *
     * @param aScriptEngineName DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public ScriptEngine getScriptEngine(String aScriptEngineName) {
        ScriptEngine lEngine = (ScriptEngine) mHshTbl.get(aScriptEngineName);

        if (lEngine == null) {
            //The script engine has not yet been loaded, so this has to be loaded now.
            msLogger.log(Level.SEVERE,
                    "The Script Engine ::" + aScriptEngineName +
                    " :: was not loaded initially");
        }

        return lEngine;
    } //get method ends.


    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public ScriptEngineManager getScriptMgr() {
        return (ScriptEngineManager) mHshTbl.get(this.SCRIPT_ENGINE_MGR);
    }

    private void init() {
        //Creating the ScriptEngineManager.
        ScriptEngineManager mScriptEngMgr = new ScriptEngineManager(this.getClass().getClassLoader());
        mHshTbl.put(this.SCRIPT_ENGINE_MGR, (Object) mScriptEngMgr);
        loadAllEngines(mScriptEngMgr);
    } // init method ends.


    /**
     * This method will load all the script engines visible to the Scripting SE and put
     * them in the repository.
     *
     * @param aMgr DOCUMENT ME!
     */
    private void loadAllEngines(ScriptEngineManager aMgr) {
        List<ScriptEngineFactory> lFactories = aMgr.getEngineFactories();
        msLogger.log(Level.INFO,
                "******************ScriptEngineFactory Info*********");

        String lEngName;
        String lEngVersion;
        String lLanguageName;
        String lLangVersion = null;

        //Getting a list of all the factories loaded..
        for (ScriptEngineFactory factory : lFactories) {
            try {
                lEngName = factory.getEngineName();

                if (!lEngName.equals("JRuby Engine")) {
                    continue;
                }

                lEngVersion = factory.getEngineVersion();
                lLanguageName = factory.getLanguageName();
                lLangVersion = factory.getLanguageVersion();

                msLogger.log(Level.INFO, "Engine Name:: " + lEngName);
                msLogger.log(Level.INFO, "Engine Version:: " + lEngVersion);
                msLogger.log(Level.INFO, "Language Name :: " + lLanguageName);
                msLogger.log(Level.INFO, "Language Version :: " + lLangVersion);

                //Storing the Engine in the Repository.   
                //                mHshTbl.put(lEngName, aMgr.getEngineByName(lEngName));
                mHshTbl.put(lEngName, factory.getScriptEngine());
            } catch (Exception ex) {
                msLogger.log(Level.SEVERE,
                        "Failure to Load all Script Engine :: \n" + ex.toString());
                ex.printStackTrace();

                continue;
            }

            msLogger.log(Level.INFO,
                    "*******FINISHED LOADING ONE SCRIPT ENGINE*********");
        } //for (ScriptEngineFactory factory: factories) ends.

    }
} //class ScriptSERepository ends.
