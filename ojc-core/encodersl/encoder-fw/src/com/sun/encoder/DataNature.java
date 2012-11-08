package com.sun.encoder;

/**
 * An enumeration describes different natures of data that encoders may handle.  
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public enum DataNature {
    
    /**
     * Indicates that an encoder processes byte based data internally
     * by nature, but it also allows passing in or outputing character based
     * data just by converting character based data into byte based data
     * before parsing or converting byte based data to character based data
     * after marshaling. So extra information (e.g., extra encoding information)
     * might be needed to process character based data using this type
     * of encoder. 
     */
    BYTE_BASED,

    /**
     * Indicates that an encoder processes character based data internally
     * by nature, but it also allows passing in or outputing byte based
     * data just by converting byte based data into character based data
     * before parsing or converting character based data to byte based data
     * after marshaling. So extra information (e.g., extra encoding information)
     * might be needed to process byte based data using this type of encoder. 
     */
    CHAR_BASED,
    
    /**
     * Indicates that an encoder can process either byte based or character
     * based data by nature.
     */
    ANY_DATA
}
