import os
import sys
import hashlib

absjoin = lambda x, y: os.path.abspath(os.path.join(x, y))


def md5(fname):
    hash_md5 = hashlib.md5()
    with open(fname, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


class Stat:

    def __init__(self, ROOT_DIR):
        self.ROOT_DIR = ROOT_DIR
        self.SCANNED = 0
        self.UNIQ = 0
        self.DUPES = 0
        self.SKIPPED = 0
        self.HASH = []
        self.COUNT = self.find_len()


    def find_len(self):
        count = 0
        for root, dirs, files in os.walk(self.ROOT_DIR):
            count += len(files)
            print("Indexing files: [{:05}]".format(count), end='\r')
            sys.stdout.flush()
        return count


    def traverse(self):
        for root, dirs, files in os.walk(self.ROOT_DIR):
            for f in files:
                self.SCANNED += 1
                fpath = absjoin(root, f)
                if(os.path.getsize(fpath) > 10*1000*1000): #10mb
                    self.SKIPPED += 1
                    continue
                else:
                    self.HASH.append(md5(fpath))
            print("Progress: {:02.2f}% [{:05}/{:05}]".format(self.SCANNED/self.COUNT * 100, self.SCANNED, self.COUNT), end='\r')
            # sys.stdout.flush()


    def info(self):
        self.HASH_SET = set(self.HASH)
        print("Unique files: {}".format(len(self.HASH_SET)))
        print("Duplicate files: {}".format(len(self.HASH) - len(self.HASH_SET)))


if __name__ == '__main__':
    s = Stat(sys.argv[1])
    s.traverse()
    print()
    s.info()

