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
 * @(#)ScreenScrapingSERepository.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.engine.screenscrapingse.process;

import java.util.Hashtable;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.glassfish.openesb.engine.screenscrapingse.I18n;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptEngineManager;


public class ScreenScrapingSERepository {

    private static ScreenScrapingSERepository msRep = null;
    private static Logger msLogger = Logger.getLogger(ScreenScrapingSERepository.class.getName());
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
    private ScreenScrapingSERepository() {
        init();
    } // ScriptSERepository() ends.


    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static ScreenScrapingSERepository GetInstance() {
        synchronized (ScreenScrapingSERepository.class) {
            if (msRep == null) {
                msRep = new ScreenScrapingSERepository();
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
            msLogger.severe(
                    I18n.loc("SSSECORE-1014: The Script Engine {0} was not loaded initially ",aScriptEngineName));
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
        msLogger.info("******************ScriptEngineFactory Info*********");

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

                msLogger.info("Engine Name:: " + lEngName);
                msLogger.info("Engine Version:: " + lEngVersion);
                msLogger.info("Language Name :: " + lLanguageName);
                msLogger.info("Language Version :: " + lLangVersion);

                //Storing the Engine in the Repository.   
                //                mHshTbl.put(lEngName, aMgr.getEngineByName(lEngName));
                mHshTbl.put(lEngName, factory.getScriptEngine());
            } catch (Exception ex) {
                msLogger.severe(I18n.loc("SSSECORE-1018: Failure to Load all Script Engine {0}", ex.getMessage()));
               
                continue;
            }

            msLogger.info("*******FINISHED LOADING ONE SCRIPT ENGINE*********");
        } //for (ScriptEngineFactory factory: factories) ends.

    }
} //class ScriptSERepository ends.
