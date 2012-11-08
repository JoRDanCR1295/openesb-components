/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.encoder.hl7.runtime.provider;

/**
 *
 * @author James
 */
public enum RawMessageModeEnum {
    /** Default behavior, parse normally */
    NONE,

    /** The whole message will be placed in a child element called "RawMessage" */
    WHOLE_MESSAGE

}
