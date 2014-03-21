package ru.spbau.turaevT.drunkard.game;

import ru.spbau.turaevT.drunkard.objects.IActiveObject;

/**
 * The <tt>Game</tt> interface provides access to control the game<p>
 * The <tt>Game</tt> interface provides method to make a step in the game<p>
 * The <tt>Game</tt> interface provides method to register an <tt>active object</tt> in the game<p>
 *
 * @see ru.spbau.turaevT.drunkard.objects.IActiveObject
 */
public interface IGame {

    /**
     * Makes a step in the game
     */
    void makeStep();

    /**
     * Registers an Active object in the game
     *
     * @param object Active object
     * @see ru.spbau.turaevT.drunkard.objects.IActiveObject
     */
    void registerActiveObject(IActiveObject object);

    /**
     * Unregisters an Active object from the game
     *
     * @param object Active object
     * @see ru.spbau.turaevT.drunkard.objects.IActiveObject
     */
    void unregisterActiveObject(IActiveObject object);
}