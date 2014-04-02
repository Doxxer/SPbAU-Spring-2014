#include <fstream>
#include <vector>
#include <iostream>
#include "FileSystem.hpp"

using std::runtime_error;
using std::string;
using std::ofstream;
using std::ifstream;

const string FileSystem::CONFIG_FILENAME = "config";

FileSystem::FileSystem(string const &root) : location(utils::pathAppend(root))
{
    read_config();
}

void FileSystem::read_config()
{
    std::ifstream config_file(utils::pathAppend(location, CONFIG_FILENAME));
    if (!config_file.is_open())
        throw runtime_error("Error occurs while reading the CONFIG file");
    config_file >> blockSize >> blocksCount;
    if (blocksCount <= 0 || blockSize < 1024)
        throw runtime_error("Config file found, but has the wrong configuration");
    usedBlocks.assign(blocksCount, 0);
}

void FileSystem::run_init()
{
    std::vector<char> zeroes(blockSize, 0);
    for (size_t i = 0; i < blocksCount; i++) {
        ofstream block(utils::pathAppend(location, std::to_string(i)));
        block.write(zeroes.data(), zeroes.size());
    }
}

void FileSystem::run_format()
{
    run_init();
    root = Directory("/");
    usedBlocks.assign(blocksCount, 0);
    usedBlocks[0] = 1;
}

void FileSystem::save()
{
    ofstream out(utils::pathAppend(location, "0"), std::ios_base::trunc | std::ios_base::binary);
    if (good()) {
        root.save(out);
    }
}

void FileSystem::load()
{
    ifstream in(utils::pathAppend(location, "0"), std::ios_base::binary);
    if (!in.is_open())
        throw runtime_error("0 block not found");
    root.load(in, location);
    root.fillUsedBlocks(usedBlocks);
    usedBlocks[0] = 1;
}

void FileSystem::run_import(const string &host_fileName, string const &fs_filename)
{
    ifstream hostFileStream(host_fileName, std::ios_base::ate | std::ios_base::binary);
    if (!hostFileStream.is_open())
        throw runtime_error("Host file not found");

    size_t host_fileSize = hostFileStream.tellg();
    size_t host_fileBlockSize = host_fileSize / blockSize + (host_fileSize % blockSize != 0);

    if (host_fileBlockSize > getFreeBlocksCount())
        throw runtime_error("Not enought space");
    hostFileStream.seekg(0);

    Path path(fs_filename);
    if (exists(path))
        throw runtime_error("File with the name '" + fs_filename + "' already exists");

    if (!isDirectory(*path.getParentPath()))
        throw runtime_error("Path to file with the name '" + fs_filename + "' not found");

    if (path.getFileName().length() > 10)
        throw runtime_error("Filename '" + path.getFileName() + "' is too long");

    File file(path.getFileName(), host_fileSize);
    size_t prevBlock = getNextFreeBlock();
    utils::write_file_to_block_with_meta(
        hostFileStream, utils::pathAppend(location, std::to_string(prevBlock)), blockSize, file);
    file.addUsedBlock(prevBlock);
    usedBlocks[prevBlock] = 1;
    while (hostFileStream.tellg() != -1) {
        size_t nextFreeBlock = getNextFreeBlock();
        utils::overwrite_nextBlockNumber(utils::pathAppend(location, std::to_string(prevBlock)),
                                         nextFreeBlock);
        utils::write_file_to_block(
            hostFileStream, utils::pathAppend(location, std::to_string(nextFreeBlock)), blockSize);
        file.addUsedBlock(nextFreeBlock);
        usedBlocks[nextFreeBlock] = 1;
        prevBlock = nextFreeBlock;
    }
    root.findLastDirectory(path)->addFile(file);
}

void FileSystem::run_export(const string &fs_filename, const string &host_filename)
{
    ofstream hostFileStream(host_filename, std::ios_base::trunc | std::ios_base::binary);
    if (!hostFileStream.is_open())
        throw runtime_error("Host file cannot be created or opened");

    Path path(fs_filename);
    if (!exists(path) || !isFile(path))
        throw runtime_error("File '" + path.getFileName() + "' not found");

    File const &file = root.findLastDirectory(path)->getFile(path.getFileName());

    for (size_t i = 0; i < file.blocks.size(); ++i)
        utils::read_from_block(hostFileStream,
                               utils::pathAppend(location, std::to_string(file.blocks[i])),
                               blockSize,
                               i == 0);
}

void FileSystem::run_ls(const string &fileName)
{
    if (fileName == root.getName()) {
        std::cout << root.get_info() << std::endl;
        return;
    }

    Path path(fileName);
    if (!exists(path))
        throw runtime_error("File " + fileName + " not found");

    if (isDirectory(path)) {
        std::cout << root.findDirectory(path)->get_info() << std::endl;
    } else {
        std::cout << root.findFile(path)->get_info() << std::endl;
    }
}

void FileSystem::run_mkdir(const string &directory)
{
    Path path(directory);
    Directory *currentDir = &root;

    for (string const &name : path.getSplittedPath()) {
        if (currentDir->existsFile(name))
            throw runtime_error("File '" + name + "' exists in directory " + currentDir->getName());

        if (name.length() > 10)
            throw runtime_error("Directory name '" + name + "' is too long");

        if (!currentDir->existsDir(name)) {
            Directory d(name);
            currentDir->addDirectory(d);
        }
        currentDir = &currentDir->getSubDir(name);
    }
}

void FileSystem::run_rm(string const &filename)
{
    Path path(filename);
    if (!exists(path))
        throw runtime_error("File '" + filename + "' not found");

    Directory *targetDirectory = root.findLastDirectory(path);
    if (isFile(path)) {
        remove_file(*targetDirectory, targetDirectory->getFile(path.getFileName()));
    } else {
        remove_dir(*targetDirectory, targetDirectory->getSubDir(path.getFileName()));
        return;
    }
}

void FileSystem::run_copy(string const &source, string const &destination)
{
    Path pathSource(source);

    if (!exists(pathSource))
        throw runtime_error("File " + source + " not found");

    Directory *sourceParentDir = root.findLastDirectory(pathSource);
    if (isFile(pathSource))
        copy_file_to(sourceParentDir->getFile(pathSource.getFileName()), destination);
    else
        copy_dir_to(sourceParentDir->getSubDir(pathSource.getFileName()), destination);
}

void FileSystem::run_move(string const &source, string const &destination)
{
    Path pathSource(source);

    if (!exists(pathSource))
        throw runtime_error("File " + source + " not found");

    Directory *sourceParentDir = root.findLastDirectory(pathSource);
    if (isFile(pathSource))
        move_file_to(
            sourceParentDir->getFile(pathSource.getFileName()), sourceParentDir, destination);
    else
        throw runtime_error("Cannot move dir");
}

void FileSystem::copy_dir_to(Directory const &directory, const string &destination)
{
    string name = directory.getName();
    if (destination == "/") {
        if (!root.existsDir(name)) {
            Directory d(name);
            root.addDirectory(d);
        }
        copy_directory(directory, root.getSubDir(name));
        return;
    }

    run_mkdir(destination);
    Path path(destination);
    if (isFile(path))
        throw runtime_error("Can't copy directory to file");

    Directory *targetDirectory = root.findDirectory(path);
    if (targetDirectory == nullptr)
        throw runtime_error("Path to '" + destination + "' not found");

    if (targetDirectory->existsFile(name))
        throw runtime_error("There is a file with name '" + name + "' in destination '" +
                            destination + "'");

    if (!targetDirectory->existsDir(name)) {
        Directory d(name);
        targetDirectory->addDirectory(d);
    }
    copy_directory(directory, targetDirectory->getSubDir(name));
}

void FileSystem::copy_file_to(File const &file, const string &destination)
{
    if (destination == "/") {
        copy_file(file, root, file.name);
        return;
    }

    Path path(destination);
    if (!exists(*path.getParentPath()))
        throw runtime_error("Path to '" + destination + "' not found");

    if (isDirectory(path)) {
        if (root.findDirectory(path)->existsFile(file.name)) {
            if (root.findDirectory(path)->getFile(file.name) == file)
                return;
        }
        copy_file(file, *root.findDirectory(path), file.name);
    } else {
        if (isFile(path) && *root.findFile(path) == file)
            return;
        if (path.getFileName().length() > 10)
            throw runtime_error("Filename '" + path.getFileName() + "' is too long");
        copy_file(file, *root.findLastDirectory(path), path.getFileName());
    }
}

void FileSystem::copy_file(File const &file, Directory &targetDirectory, string const &newName)
{
    if (targetDirectory.existsFile(newName))
        remove_file(targetDirectory, targetDirectory.getFile(newName));

    if (targetDirectory.existsDir(file.name))
        throw runtime_error("Cannot overwrite directory '" + file.name + " with non-directory");

    size_t host_fileBlockSize = file.size / blockSize + (file.size % blockSize != 0);
    if (host_fileBlockSize > getFreeBlocksCount())
        throw runtime_error("Not enought space");

    File newFile(newName, file.size);
    size_t prevBlock = getNextFreeBlock();
    utils::copy_block_to_block(utils::pathAppend(location, std::to_string(file.blocks[0])),
                               utils::pathAppend(location, std::to_string(prevBlock)),
                               blockSize);
    utils::overwrite_metadata(newFile, utils::pathAppend(location, std::to_string(prevBlock)));
    newFile.addUsedBlock(prevBlock);
    usedBlocks[prevBlock] = 1;
    for (size_t i = 1; i < file.blocks.size(); ++i) {
        size_t nextFreeBlock = getNextFreeBlock();
        utils::overwrite_nextBlockNumber(utils::pathAppend(location, std::to_string(prevBlock)),
                                         nextFreeBlock);
        utils::copy_block_to_block(utils::pathAppend(location, std::to_string(file.blocks[i])),
                                   utils::pathAppend(location, std::to_string(nextFreeBlock)),
                                   blockSize);
        newFile.addUsedBlock(nextFreeBlock);
        usedBlocks[nextFreeBlock] = 1;
        prevBlock = nextFreeBlock;
    }
    targetDirectory.addFile(newFile);
}

void FileSystem::copy_directory(Directory const &directory, Directory &targetDirectory)
{
    for (auto d : directory.getAllDirectories()) {
        Directory newd(d.getName());
        targetDirectory.addDirectory(newd);
        copy_directory(d, targetDirectory.getSubDir(newd.getName()));
    }
    for (auto f : directory.getAllFiles()) {
        copy_file(f, targetDirectory, f.name);
    }
}

void FileSystem::remove_file(Directory &dir, File &f)
{
    for (auto block : f.blocks) {
        usedBlocks[block] = 0;
    }
    dir.removeFile(f);
}

void FileSystem::remove_dir(Directory &dir, Directory &target)
{
    for (auto t : target.getAllDirectories()) {
        remove_dir(target, t);
    }
    for (auto f : target.getAllFiles()) {
        remove_file(target, f);
    }
    dir.removeDir(target);
}

void FileSystem::move_file_to(File const &file, Directory *parentDir, const string &destination)
{
    if (destination == "/") {
        move_file(file, parentDir, root, file.name);
        return;
    }

    Path path(destination);
    if (!exists(*path.getParentPath()))
        throw runtime_error("Path to '" + destination + "' not found");

    if (isDirectory(path)) {
        if (root.findDirectory(path)->existsFile(file.name)) {
            if (root.findDirectory(path)->getFile(file.name) == file)
                return;
        }
        move_file(file, parentDir, *root.findDirectory(path), file.name);
    } else {
        if (isFile(path) && *root.findFile(path) == file)
            return;
        move_file(file, parentDir, *root.findLastDirectory(path), path.getFileName());
    }
}

void FileSystem::move_file(File const &file,
                           Directory *parentDir,
                           Directory &targetDirectory,
                           string const &newName)
{
    File f(file);
    f.name = newName;
    targetDirectory.addFile(f);
    parentDir->removeFile(parentDir->getFile(file.name));

    string firstBlock = utils::pathAppend(location, std::to_string(f.blocks[0]));
    utils::overwrite_metadata(f, firstBlock);
    if (f.blocks.size() > 1)
        utils::overwrite_nextBlockNumber(firstBlock, f.blocks[1]);
}
