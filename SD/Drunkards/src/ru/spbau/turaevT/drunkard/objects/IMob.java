package ru.spbau.turaevT.drunkard.objects;

public interface IMob extends IActiveObject, IGameObject {
    void processColliding(Column object);
    void processColliding(Drunkard object);
    void processColliding(Bottle object);
}
