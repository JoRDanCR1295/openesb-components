/*
 * ScriptSERepository.java
 *
 * Created on February 23, 2007, 3:10 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.engine.scriptse.process;

import java.util.Hashtable;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptEngineManager;


/**
 * DOCUMENT ME!
 *
 * @author Prashanth B.R
 */
public class ScriptseRepository {
    private static ScriptseRepository msRep = null;
    private static Logger msLogger = Logger.getLogger(ScriptseRepository.class.getName());
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
    private ScriptseRepository() {
        init();
    } // ScriptSERepository() ends.

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static ScriptseRepository GetInstance() {
        synchronized (ScriptseRepository.class) {
            if (msRep == null) {
                msRep = new ScriptseRepository();
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
            msLogger.log(
                Level.SEVERE,
                "The Script Engine ::" + aScriptEngineName + " :: was not loaded initially"
            );
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
        ScriptEngineManager mScriptEngMgr = new ScriptEngineManager(
                this.getClass().getClassLoader()
            );
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
        msLogger.log(Level.INFO, "******************ScriptEngineFactory Info*********");

        String lEngName;
        String lEngVersion;
        String lLanguageName;
        String lLangVersion = null;

        //Getting a list of all the factories loaded..
        for (ScriptEngineFactory factory : lFactories) {
            try {
                lEngName = factory.getEngineName();
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
            } catch (Throwable ex) {
                msLogger.log(
                    Level.SEVERE, "Failure to Load all Script Engine :: \n" + ex.toString()
                );
                ex.printStackTrace();

                continue;
            }

            msLogger.log(Level.INFO, "*******FINISHED LOADING ONE SCRIPT ENGINE*********");
        } //for (ScriptEngineFactory factory: factories) ends.
    }
} //class ScriptSERepository ends.
